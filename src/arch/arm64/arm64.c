/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>
#include <stdint.h>
#include <sys/stat.h>
#include <unistd.h>

#include <allocate.h>
#include <error_or.h>
#include <options.h>
#include <process.h>

#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)
SHARED_ALLOCATOR_IMPL(arm64)

OpcodeMap get_opcode_map(type_id type)
{
    OpcodeMap ret = { 0 };
    ret.type = type;
    size_t sz = typeid_sizeof(type);
    bool   un_signed = false;
    if (typeid_kind(type) == TK_PRIMITIVE) {
        BuiltinType builtin_type = typeid_builtin_type(type);
        uint16_t    type_meta = type >> 16;
        un_signed = type_meta & 0x0100;
    }
    switch (sz) {
    case 0x01:
        ret.load_opcode = (un_signed) ? "ldrb" : "ldrsb";
        ret.store_opcode = "strb";
        ret.reg_width = RW_32;
        break;
    case 0x02:
        ret.load_opcode = (un_signed) ? "ldrh" : "ldrsh";
        ret.store_opcode = "strh";
        ret.reg_width = RW_32;
        break;
    case 0x04:
        ret.load_opcode = "ldr";
        ret.store_opcode = "str";
        ret.reg_width = RW_32;
        break;
    default:
        ret.load_opcode = "ldr";
        ret.store_opcode = "str";
        ret.reg_width = RW_64;
        break;
    }
    return ret;
}

StringView value_location_to_string(ValueLocation loc, Allocator *allocator)
{
    StringBuilder sb = sb_acreate(allocator);
    sb_append_cstr(&sb, ValueLocationKind_name(loc.kind));
    sb_append_cstr(&sb, " ");
    switch (loc.kind) {
    case VLK_POINTER:
        sb_printf(&sb, "[%s, #%d]", x_reg(loc.pointer.reg), loc.pointer.offset);
        break;
    case VLK_REGISTER:
        sb_printf(&sb, "%s",
            reg_with_width(loc.reg, (typeid_sizeof(loc.type) == 8) ? RW_64 : RW_32));
        break;
    case VLK_REGISTER_RANGE:
        sb_printf(&sb, "%s-%s",
            reg_with_width(loc.range.start, (typeid_sizeof(loc.type) == 8) ? RW_64 : RW_32),
            reg_with_width(loc.range.end, (typeid_sizeof(loc.type) == 8) ? RW_64 : RW_32));
        break;
    case VLK_STACK:
        sb_printf(&sb, "[SP, #%ld]", loc.offset);
        break;
    case VLK_SYMBOL:
        sb_printf(&sb, "%.*s", SV_ARG(loc.symbol));
        break;
    default:
        UNREACHABLE();
    }
    sb_printf(&sb, " (%.*s, %zu)", SV_ARG(typeid_name(loc.type)), typeid_sizeof(loc.type));
    return sb.view;
}

ErrorOrInt output_arm64(IRProgram *program)
{
    ARM64Context *ctx = generate_arm64(program);
    Assembly     *main = NULL;
    for (size_t ix = 0; ix < ctx->assemblies.num; ++ix) {
        Assembly *assembly = ctx->assemblies.elements + ix;
        if (assembly_has_main(assembly)) {
            main = assembly;
            break;
        }
    }
    if (!main) {
        fatal("No main() function found");
    }

    if (mkdir(".scribble", 0755) && (errno != EEXIST)) {
        fatal("Could not create .scribble build directory");
    }

#if 0
    assembly_new_function(main);
    for (size_t ix = 0; ix < ctx->assemblies.num; ++ix) {
        Assembly *assembly = ctx->assemblies.elements + ix;
        size_t      fnc_ix = da_append_ARM64Function(
            &main->functions,
            (ARM64Function) {
                     .assembly = assembly,
                     .function = function,
                     .scope.kind = SK_FUNCTION,
                     .scope.up = &assembly->scope,
            });
        ARM64Function *arm_function = assembly->functions.elements + fnc_ix;
        if (!assembly_has_static(assembly) || !assembly_has_exports(assembly)) {
            continue;
        }
        assembly_add_instruction(main, "bl", "static_%.*s", SV_ARG(assembly->name));
    }
    code_close_function(main->active, sv_from("static_initializer"), 0);
#endif

    StringList modules = sl_acreate(get_allocator());
    for (size_t ix = 0; ix < ctx->assemblies.num; ++ix) {
        Assembly *assembly = ctx->assemblies.elements + ix;
        if (assembly_has_exports(assembly)) {
            StringView bare_file_name = assembly->module->name;
            int        slash = sv_last(bare_file_name, '/');
            if (slash > 0) {
                bare_file_name = sv_lchop(bare_file_name, slash + 1);
            }
            int dot = sv_last(bare_file_name, '.');
            if (dot > 0) {
                bare_file_name = sv_rchop(bare_file_name, bare_file_name.length - dot);
            }
            bare_file_name = sv_aprintf(get_allocator(), ".scribble/%.*s", SV_ARG(bare_file_name));

            assembly_save_and_assemble(assembly, bare_file_name);
            if (!has_option("keep-assembly")) {
                unlink(sv_cstr(sv_aprintf(get_allocator(), "%.*s.s", SV_ARG(bare_file_name))));
            }
            StringBuilder obj_file = sb_acreate(get_allocator());
            sb_printf(&obj_file, "%.*s.o", SV_ARG(bare_file_name));
            sl_push(&modules, obj_file.view);
        }
    }

    int result = 0;
    if (modules.size > 0) {
        StringView scribble_dir = get_option(sv_from("scribble-dir"));
        StringView bin_name = program->name;
        int        slash = sv_last(bin_name, '/');
        if (slash >= 0) {
            bin_name = sv_lchop(bin_name, slash + 1);
        }
        int dot = sv_last(bin_name, '.');
        if (dot > 0) {
            bin_name = sv_rchop(bin_name, bin_name.length - dot);
        }

#ifdef IS_APPLE
        StringView sdk_path = { 0 }; // "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk";
        if (sv_empty(sdk_path)) {
            Process *p = process_create(sv_from("xcrun"), "-sdk", "macosx", "--show-sdk-path");
            MUST_VOID(Int, process_execute(p))
            sdk_path = sv_strip(p->out.view);
        }

        StringList ld_args = sl_acreate(get_allocator());
        sl_push(&ld_args, sv_from("-o"));
        sl_push(&ld_args, bin_name);
        sl_push(&ld_args, sv_from("-lscb_base"));
        sl_push(&ld_args, sv_from("-lscribblert"));
        sl_push(&ld_args, sv_from("-lSystem"));
        sl_push(&ld_args, sv_from("-syslibroot"));
        sl_push(&ld_args, sdk_path);
        sl_push(&ld_args, sv_from("-e"));
        sl_push(&ld_args, sv_from("_start"));
        sl_push(&ld_args, sv_from("-arch"));
        sl_push(&ld_args, sv_from("arm64"));
        sl_push(&ld_args, sv_aprintf(get_allocator(), "-L%.*s/lib", SV_ARG(scribble_dir)));
        sl_extend(&ld_args, &modules);

        //        std::vector<std::string> ld_args = { "-o", config.main(), "-loblrt",
        //            "-lSystem", "-syslibroot", sdk_path, "-e", "_start", "-arch", "arm64",
        //            format("-L{}/lib", obl_dir) };
        //        for (auto& m : modules)
        //            ld_args.push_back(m);

        MUST(Int, int, ld_result, execute_sl(sv_from("ld"), &ld_args))
        if (ld_result) {
            fatal("ld failed with exit code %d", ld_result);
        }
#elif defined(IS_LINUX)
        StringList ld_args = sl_acreate(get_allocator());
        sl_push(&ld_args, sv_from("-o"));
        sl_push(&ld_args, bin_name);
        sl_push(&ld_args, sv_from("-lscribblert"));
        sl_push(&ld_args, sv_from("-e"));
        sl_push(&ld_args, sv_from("_start"));
        sl_push(&ld_args, sv_from("-A"));
        sl_push(&ld_args, sv_from("aarch64"));
        sl_push(&ld_args, sv_aprintf(get_allocator(), "-L%.*s/lib", SV_ARG(scribble_dir)));
        sl_extend(&ld_args, &modules);
        MUST(Int, int, ld_result, execute_sl(sv_from("ld"), &ld_args))
        if (ld_result) {
            fatal("ld failed with exit code %d", ld_result);
        }
#endif
        if (has_option("run-binary")) {
            StringBuilder run_cmd = sb_acreate(get_allocator());
            sb_printf(&run_cmd, "./%s", bin_name);
            ErrorOrInt exit_code_or_error = execute(run_cmd.view);
            if (ErrorOrInt_is_error(exit_code_or_error)) {
                Error e = exit_code_or_error.error;
                ERROR(Int, ProcessError, e.code, "Program execution failed: %s", Error_to_string(e));
            } else {
                result = exit_code_or_error.value;
            }
        }
    }
    RETURN(Int, result);
}
