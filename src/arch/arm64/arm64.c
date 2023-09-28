/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include <allocate.h>
#include <options.h>
#include <process.h>

#include <arch/arm64/arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)
SHARED_ALLOCATOR_IMPL(arm64)

OptionalOpcodeMap get_opcode_map(type_id type)
{
    OptionalOpcodeMap ret = { 0 };
    ret.has_value = false;
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        ret.has_value = true;
        ret.value.type = typeid_builtin_type(type);
        uint16_t type_meta = type >> 16;
        bool     un_signed = type_meta & 0x0100;
        switch ((type >> 16) & 0x00FF) {
        case 0x08:
            ret.value.load_opcode = (un_signed) ? "ldrb" : "ldrsb";
            ret.value.store_opcode = "strb";
            ret.value.reg_width = "w";
            break;
        case 0x10:
            ret.value.load_opcode = (un_signed) ? "ldrh" : "ldrsh";
            ret.value.store_opcode = "strh";
            ret.value.reg_width = "w";
            break;
        case 0x20:
            ret.value.load_opcode = "ldr";
            ret.value.store_opcode = "str";
            ret.value.reg_width = "w";
            break;
        case 0x40:
            ret.value.load_opcode = "ldr";
            ret.value.store_opcode = "str";
            ret.value.reg_width = "x";
            break;
        default:
            ret.has_value = false;
            break;
        }
    } break;
    default:
        break;
    }
    return ret;
}

ErrorOrInt output_arm64(IRProgram *program)
{
    ARM64Context *ctx = generate_arm64(program, get_allocator());
    Assembly     *main = NULL;
    for (Assembly *assembly = ctx->assemblies; assembly; assembly = assembly->next) {
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

    assembly_enter_function(main, sv_from("static_initializer"), 0);
    for (Assembly *assembly = ctx->assemblies; assembly; assembly = assembly->next) {
        if (!assembly_has_static(assembly) || !assembly_has_exports(assembly)) {
            continue;
        }
        assembly_add_instruction(main, "bl", "static_%.*s", SV_ARG(assembly->name));
    }
    assembly_leave_function(main, 0);

    StringList modules = sl_acreate(get_allocator());
    for (Assembly *assembly = ctx->assemblies; assembly; assembly = assembly->next) {
        if (assembly_has_exports(assembly)) {
            StringView bare_file_name = assembly->name;
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

        StringView sdk_path = { 0 }; // "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk";
        if (sv_empty(sdk_path)) {
            Process *p = process_create(sv_from("xcrun"), "-sdk", "macosx", "--show-sdk-path");
            MUST_VOID(Int, process_execute(p))
            sdk_path = sv_strip(p->out.view);
        }

        StringList ld_args = sl_acreate(get_allocator());
        sl_push(&ld_args, sv_from("-o"));
        sl_push(&ld_args, bin_name);
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
