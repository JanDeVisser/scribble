/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <intermediate.h>
#include <optional.h>
#include <sv.h>
#include <type.h>

#ifndef __ARM64_H__
#define __ARM64_H__

#define ROOT_MODULE_NAME sv_from("$root")

typedef struct code {
    Allocator     *allocator;
    StringBuilder  prolog;
    StringBuilder  code;
    StringBuilder  epilog;
    StringBuilder *active;
} Code;

typedef struct string_id {
    StringView        string;
    size_t            id;
    struct string_id *next;
} StringID;

typedef struct assembly {
    Allocator       *allocator;
    StringView       name;
    Code            *code;
    Code            *statik;
    Code            *active;
    StringBuilder    text;
    StringBuilder    data;
    bool             has_exports;
    bool             has_main;
    bool             has_static;
    StringID        *strings;
    struct assembly *next;
} Assembly;

#define VARIABLEADDRESSTYPES(S) \
    S(STACK)                    \
    S(STATIC)                   \
    S(GLOBAL)                   \
    S(AGGREGATE_COMPONENT)      \
    S(ARRAY_ELEMENT)

typedef enum variable_address_type {
#undef VARIABLEADDRESSTYPE
#define VARIABLEADDRESSTYPE(type) VAT_##type,
    VARIABLEADDRESSTYPES(VARIABLEADDRESSTYPE)
#undef VARIABLEADDRESSTYPE
} ARM64VariableAddressType;

static inline char const *VariableAddressType_name(ARM64VariableAddressType type)
{
    switch (type) {
#undef VARIABLEADDRESSTYPE
#define VARIABLEADDRESSTYPE(type) \
    case VAT_##type:              \
        return #type;
        VARIABLEADDRESSTYPES(VARIABLEADDRESSTYPE)
#undef VARIABLEADDRESSTYPE
    default:
        UNREACHABLE();
    }
}

typedef struct arm64_variable_address {
    ARM64VariableAddressType type;
    union {
        struct {
            size_t offset;
        } stack_address;
        struct {
            StringView label;
        } static_address;
        struct {
            struct arm64_variable_address *aggregate;
            size_t                         offset;
        } aggregate_component;
        struct {
            struct arm64_variable_address *array;
            size_t                         element_size;
        } array_component;
    };
} ARM64VariableAddress;

#define PARAMETERPASSINGMETHODS(S) \
    S(REGISTER)                    \
    S(STACK)

typedef enum parameter_passing_method {
#undef PARAMETERPASSINGMETHOD
#define PARAMETERPASSINGMETHOD(ppm) PPM_##ppm,
    PARAMETERPASSINGMETHODS(PARAMETERPASSINGMETHOD)
#undef PARAMETERPASSINGMETHOD
} ParameterPassingMethod;

static inline char const *ParameterPassingMethod_name(ParameterPassingMethod mth)
{
    switch (mth) {
#undef PARAMETERPASSINGMETHOD
#define PARAMETERPASSINGMETHOD(method) \
    case PPM_##method:                 \
        return #method;
        PARAMETERPASSINGMETHODS(PARAMETERPASSINGMETHOD)
#undef PARAMETERPASSINGMETHOD
    default:
        UNREACHABLE();
    }
}

typedef struct arm64_var_decl {
    IRVarDecl             *var_decl;
    ARM64VariableAddress   address;
    ParameterPassingMethod method;
    size_t                 where;
} ARM64VarDecl;

typedef struct arm64_function {
    Allocator    *allocator;
    IRFunction   *function;
    size_t        num_parameters;
    ARM64VarDecl *parameters;
    union {
        struct {
            size_t nsaa;
            size_t stack_depth;
        } scribble;
        struct {
            StringView stub_name;
            size_t     nsaa;
        } native;
    };
} ARM64Function;

typedef struct int_list {
    size_t           value;
    struct int_list *next;
} IntList;

typedef struct function_list {
    ARM64Function        *function;
    struct function_list *next;
} FunctionList;

typedef struct arm64_context {
    Allocator     *allocator;
    IRProgram     *program;
    Assembly      *assemblies;
    Assembly      *assembly;
    IntList       *stack_depths;
    size_t         stack_allocated;
    size_t         num_functions;
    ARM64Function *functions;
    ARM64Function *function;
} ARM64Context;

typedef struct opcode_map {
    PrimitiveType type;
    char const   *load_opcode;
    char const   *store_opcode;
    char const   *reg_width;
} OpcodeMap;

OPTIONAL(OpcodeMap)

extern OptionalOpcodeMap get_opcode_map(type_id type);
extern Code             *code_acreate(Allocator *allocator, StringView prolog, StringView epilog);
extern void              code_add_instruction(Code *code, char const *opcode, char const *arg_fmt, ...);
extern void              code_vadd_instruction(Code *code, char const *opcode, char const *arg_fmt, va_list args);
extern void              code_add_text(Code *code, char const *text, ...);
extern void              code_vadd_text(Code *code, char const *text, va_list args);
extern void              code_add_label(Code *code, StringView label);
extern void              code_add_directive(Code *code, StringView directive, StringView args);
extern void              code_add_comment(Code *code, StringView comment);
extern StringView        code_to_string(Code *code);
extern bool              code_empty(Code *code);
extern bool              code_has_text(Code *code);
extern void              code_enter_function(Code *code, StringView name, size_t stack_depth);
extern void              code_leave_function(Code *code, size_t stack_depth);
extern void              code_select_prolog(Code *code);
extern void              code_select_epilog(Code *code);
extern void              code_select_code(Code *code);
extern Assembly         *assembly_acreate(Allocator *allocator, StringView name);
extern void              assembly_add_instruction(Assembly *assembly, char const *opcode, char const *arg_fmt, ...);
extern void              assembly_add_text(Assembly *assembly, char const *text, ...);
extern void              assembly_vadd_text(Assembly *assembly, char const *text, va_list args);
extern void              assembly_add_text_sv(Assembly *assembly, StringView text);
extern void              assembly_add_label(Assembly *assembly, StringView label);
extern void              assembly_add_comment(Assembly *assembly, char const *comment, ...);
extern void              assembly_add_directive(Assembly *assembly, StringView directive, StringView args);
extern size_t            assembly_add_string(Assembly *assembly, StringView str);
extern void              assembly_add_data(Assembly *assembly, StringView label, bool global, StringView type, bool is_static, StringView value);
extern StringView        assembly_to_string(Assembly *assembly);
extern void              assembly_syscall(Assembly *assembly, int id);
extern void              assembly_syscall1(Assembly *assembly, int id, uint64_t arg1);
extern void              assembly_syscall2(Assembly *assembly, int id, uint64_t arg1, uint64_t arg2);
extern void              assembly_syscall3(Assembly *assembly, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3);
extern void              assembly_syscall4(Assembly *assembly, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3, uint64_t arg4);
extern bool              assembly_has_exports(Assembly *assembly);
extern bool              assembly_has_main(Assembly *assembly);
extern bool              assembly_has_static(Assembly *assembly);
extern Code             *assembly_static_initializer(Assembly *assembly);
extern void              assembly_select_code(Assembly *assembly);
extern void              assembly_select_static(Assembly *assembly);
extern void              assembly_enter_function(Assembly *assembly, StringView fnc, size_t stackdepth);
extern void              assembly_leave_function(Assembly *assembly, size_t stackdepth);
extern void              assembly_save_and_assemble(Assembly *assembly, StringView file_name);
extern void              assembly_push(Assembly *assembly, char const *reg);
extern void              assembly_pop(Assembly *assembly, char const *reg);
extern void              assembly_write_char(Assembly *assembly, int fd, char ch);
extern StringView        arm64var_decl_to_string(ARM64VarDecl *var, Allocator *allocator);
extern ARM64Function    *arm64function_acreate(Allocator *allocator, IRFunction *function, size_t nsaa, size_t stack_depth);
extern StringView        arm64function_label(ARM64Function *function);
extern StringView        arm64function_to_string(ARM64Function *function);
extern void              arm64variable_address_store_variable(ARM64VariableAddress *address, type_id type, ARM64Context *ctx, int from);
extern void              arm64variable_address_load_variable(ARM64VariableAddress *address, type_id type, ARM64Context *ctx, int target);
extern void              arm64variable_address_prepare_pointer(ARM64VariableAddress *address, ARM64Context *ctx);
extern StringView        arm64variable_address_to_string(ARM64VariableAddress *address, Allocator *allocator);
extern ARM64Context     *arm64context_acreate(Allocator *allocator);
extern void              arm64context_add_module(ARM64Context *ctx, StringView name);
extern void              arm64context_zero_initialize(ARM64Context *ctx, type_id type, int offset);
extern void              arm64context_load_variable(ARM64Context *ctx, type_id type, size_t offset, int target);
extern void              arm64context_store_variable(ARM64Context *ctx, type_id type, size_t offset, int from);
extern void              arm64context_enter_function(ARM64Context *ctx, ARM64Function *func);
extern void              arm64context_function_return(ARM64Context *ctx);
extern void              arm64context_leave_function(ARM64Context *ctx);
extern void              arm64context_set_stack_depth(ARM64Context *ctx, size_t depth);
extern void              arm64context_pop_stack_depth(ARM64Context *ctx);
extern size_t            arm64context_get_stack_depth(ARM64Context *ctx);
extern ARM64Function    *arm64context_function_by_name(ARM64Context *ctx, StringView name);
extern ARM64Context     *generate_arm64(IRProgram *program, Allocator *allocator);
extern ErrorOrInt        output_arm64(IRProgram *program);

#endif /* __ARM64_H__ */
