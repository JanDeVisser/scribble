/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <config.h>
#include <intermediate.h>
#include <optional.h>
#include <sv.h>
#include <type.h>

#ifndef __ARM64_H__
#define __ARM64_H__

#define ROOT_MODULE_NAME sv_from("$root")

#ifdef IS_APPLE

#define SYSCALL_REG "x16"
#define SYSCALL_EXIT 0X0001
#define SYSCALL_FORK 0X0002
#define SYSCALL_READ 0X0003
#define SYSCALL_WRITE 0X0004
#define SYSCALL_OPEN 0X0005
#define SYSCALL_CLOSE 0X0006
#define SYSCALL_WAIT4 0X0007
#define SYSCALL_STAT 0X0153
#define SYSCALL_MMAP 0X00C5

#elif defined(IS_LINUX)

#define SYSCALL_REG "w8"
#define SYSCALL_SYSCALL 0XFFFF
#define SYSCALL_CLONE 0X00DC
#define SYSCALL_EXIT 0X005D
#define SYSCALL_READ 0X0063
#define SYSCALL_WRITE 0X0040
#define SYSCALL_OPENAT 0X0038
#define SYSCALL_CLOSE 0X0039
#define SYSCALL_WAIT4 0X0104
#define SYSCALL_STATX 0X0123
#define SYSCALL_MMAP 0X00DE

#endif

typedef enum register_width {
    RW_32,
    RW_64,
} RegisterWidth;

#define ARM64REGISTERS(S) \
    S(X0, "x0", 0)        \
    S(X1, "x1", 1)        \
    S(X2, "x2", 2)        \
    S(X3, "x3", 3)        \
    S(X4, "x4", 4)        \
    S(X5, "x5", 5)        \
    S(X6, "x6", 6)        \
    S(X7, "x7", 7)        \
    S(X8, "x8", 8)        \
    S(X9, "x9", 9)        \
    S(X10, "x10", 10)     \
    S(X11, "x11", 11)     \
    S(X12, "x12", 12)     \
    S(X13, "x13", 13)     \
    S(X14, "x14", 14)     \
    S(X15, "x15", 15)     \
    S(X16, "x16", 16)     \
    S(X17, "x17", 17)     \
    S(X18, "x18", 18)     \
    S(X19, "x19", 19)     \
    S(X20, "x20", 20)     \
    S(X21, "x21", 21)     \
    S(X22, "x22", 22)     \
    S(X23, "x23", 23)     \
    S(X24, "x24", 24)     \
    S(X25, "x25", 25)     \
    S(X26, "x26", 26)     \
    S(X27, "x27", 27)     \
    S(X28, "x28", 28)     \
    S(FP, "fp", 29)       \
    S(LR, "lr", 30)       \
    S(XZR, "xzr", 31)     \
    S(W0, "w0", 32)       \
    S(W1, "w1", 33)       \
    S(W2, "w2", 34)       \
    S(W3, "w3", 35)       \
    S(W4, "w4", 36)       \
    S(W5, "w5", 37)       \
    S(W6, "w6", 38)       \
    S(W7, "w7", 39)       \
    S(W8, "w8", 40)       \
    S(W9, "w9", 41)       \
    S(W10, "w10", 42)     \
    S(W11, "w11", 43)     \
    S(W12, "w12", 44)     \
    S(W13, "w13", 45)     \
    S(W14, "w14", 46)     \
    S(W15, "w15", 47)     \
    S(W16, "w16", 48)     \
    S(W17, "w17", 49)     \
    S(W18, "w18", 50)     \
    S(W19, "w19", 51)     \
    S(W20, "w20", 52)     \
    S(W21, "w21", 53)     \
    S(W22, "w22", 54)     \
    S(W23, "w23", 55)     \
    S(W24, "w24", 56)     \
    S(W25, "w25", 57)     \
    S(W26, "w26", 58)     \
    S(W27, "w27", 59)     \
    S(W28, "w28", 60)     \
    S(SP, "sp", 61)       \
    S(PC, "pc", 62)       \
    S(WZR, "wzr", 63)     \
    S(V0, "v0", 64)       \
    S(V1, "v1", 65)       \
    S(V2, "v2", 66)       \
    S(V3, "v3", 67)       \
    S(V4, "v4", 68)       \
    S(V5, "v5", 69)       \
    S(V6, "v6", 70)       \
    S(V7, "v7", 71)       \
    S(V8, "v8", 72)       \
    S(V9, "v9", 73)       \
    S(V10, "v10", 74)     \
    S(V11, "v11", 75)     \
    S(V12, "v12", 76)     \
    S(V13, "v13", 77)     \
    S(V14, "v14", 78)     \
    S(V15, "v15", 79)     \
    S(V16, "v16", 80)     \
    S(V17, "v17", 81)     \
    S(V18, "v18", 82)     \
    S(V19, "v19", 83)     \
    S(V20, "v20", 84)     \
    S(V21, "v21", 85)     \
    S(V22, "v22", 86)     \
    S(V23, "v23", 87)     \
    S(V24, "v24", 88)     \
    S(V25, "v25", 89)     \
    S(V26, "v26", 90)     \
    S(V27, "v27", 91)     \
    S(V28, "v28", 92)     \
    S(V29, "v29", 93)     \
    S(V30, "v30", 94)     \
    S(V31, "v31", 95)

typedef enum arm64_register {
#undef ARM64REGISTER
#define ARM64REGISTER(r, s, n) REG_##r = n,
    ARM64REGISTERS(ARM64REGISTER)
#undef ARM64REGISTER
} Register;

static inline char const *reg(Register reg)
{
    switch (reg) {
#undef ARM64REGISTER
#define ARM64REGISTER(r, s, n) \
    case REG_##r:              \
        return s;
        ARM64REGISTERS(ARM64REGISTER)
#undef ARM64REGISTER
    default:
        UNREACHABLE();
    }
}

static inline char const *x_reg(Register r)
{
    assert((int) r <= (int) REG_WZR);
    return (r != REG_PC) ? reg(r & 0x1F) : reg(r);
}

static inline char const *w_reg(Register r)
{
    assert((int) r <= (int) REG_WZR);
    assert(r != REG_FP && r != REG_LR && r != REG_SP && r != REG_PC);
    return reg(r | 0x20);
}

static inline char const *reg_with_width(Register r, RegisterWidth w)
{
    assert((int) r <= (int) REG_WZR);
    if (r != REG_FP && r != REG_LR && r != REG_SP && r != REG_PC) {
        return (w == RW_32) ? w_reg(r) : x_reg(r);
    } else {
        return reg(r);
    }
}

static inline char const *v_reg(Register r)
{
    assert((int) r >= REG_V0 && (int) r <= (int) REG_V31);
    return reg(r);
}

#define VALUELOCATIONKINDS(S) \
    S(POINTER)                \
    S(REGISTER)               \
    S(REGISTER_RANGE)         \
    S(STACK)                  \
    S(SYMBOL)

typedef enum value_location_kind {
#undef VALUELOCATIONKIND
#define VALUELOCATIONKIND(kind) VLK_##kind,
    VALUELOCATIONKINDS(VALUELOCATIONKIND)
#undef VALUELOCATIONKIND
} ValueLocationKind;

static inline char const *ValueLocationKind_name(ValueLocationKind kind)
{
    switch (kind) {
#undef VALUELOCATIONKIND
#define VALUELOCATIONKIND(kind) \
    case VLK_##kind:            \
        return #kind;
        VALUELOCATIONKINDS(VALUELOCATIONKIND)
#undef VALUELOCATIONKIND
    default:
        UNREACHABLE();
    }
}

typedef struct value_location {
    type_id           type;
    ValueLocationKind kind;
    union {
        struct {
            Register reg;
            int64_t  offset;
        } pointer;
        Register reg;
        struct {
            Register start;
            Register end;
        } range;
        int64_t    offset;
        StringView symbol;
    };
    struct value_location *next;
} ValueLocation;

OPTIONAL(ValueLocation);

typedef struct code {
    struct arm64_function *function;
    StringBuilder          prolog;
    StringBuilder          code;
    StringBuilder          epilog;
    StringBuilder         *active;
} Code;

typedef struct string_id {
    StringView        string;
    size_t            id;
    struct string_id *next;
} StringID;

#define VARIABLEKINDS(S)   \
    S(PARAMETER)           \
    S(LOCAL)               \
    S(STATIC)              \
    S(GLOBAL)              \
    S(AGGREGATE_COMPONENT) \
    S(ARRAY_ELEMENT)

typedef enum variable_kind {
#undef VARIABLEKIND
#define VARIABLEKIND(type) VK_##type,
    VARIABLEKINDS(VARIABLEKIND)
#undef VARIABLEKIND
} ARM64VariableKind;

static inline char const *VariableKind_name(ARM64VariableKind type)
{
    switch (type) {
#undef VARIABLEKIND
#define VARIABLEKIND(type) \
    case VK_##type:        \
        return #type;
        VARIABLEKINDS(VARIABLEKIND)
#undef VARIABLEKIND
    default:
        UNREACHABLE();
    }
}

#define PARAMETERPASSINGMETHODS(S) \
    S(REGISTER)                    \
    S(STACK)                       \
    S(POINTER)                     \
    S(POINTER_STACK)

typedef enum parameter_passing_method {
#undef PARAMETERPASSINGMETHOD
#define PARAMETERPASSINGMETHOD(ppm) PPM_##ppm,
    PARAMETERPASSINGMETHODS(PARAMETERPASSINGMETHOD)
#undef PARAMETERPASSINGMETHOD
} ParameterPassingMethod;

static inline char const *
ParameterPassingMethod_name(ParameterPassingMethod mth)
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

typedef struct arm64_variable {
    struct arm64_function *function;
    ARM64VariableKind      kind;
    IRVarDecl              var_decl;
    union {
        struct {
            int64_t                offset;
            ParameterPassingMethod method;
            union {
                Register reg;
                int64_t  nsaa_offset;
            };
        } parameter;
        struct {
            int64_t offset;
        } local_address;
        struct {
            StringView label;
        } static_address;
        struct {
            struct arm64_variable *aggregate;
            int64_t                offset;
        } aggregate_component;
        struct {
            struct arm64_variable *array;
            int64_t                element_size;
        } array_component;
    };
} ARM64Variable;

DA(ARM64Variable)

typedef enum scope_kind {
    SK_GLOBAL,
    SK_STATIC,
    SK_FUNCTION,
    SK_BLOCK,
} ScopeKind;

typedef struct arm64_scope {
    ScopeKind           kind;
    DA_ARM64Variable    variables;
    IROperation        *operation;
    int64_t             depth;
    struct arm64_scope *up;
    struct arm64_scope *scopes;
    struct arm64_scope *current;
    ValueLocation      *expression_stack;
    struct arm64_scope *next;
} ARM64Scope;

typedef struct arm64_function {
    struct assembly *assembly;
    IRFunction      *function;
    StringView       label;
    ARM64Scope       scope;
    int64_t          nsaa;
    union {
        struct {
            ARM64Scope *current_scope;
            Code       *code;
            bool        registers[(int) REG_V31 + 1];
            bool        callee_saved[(int) REG_FP - (int) REG_X19];
        } scribble;
        struct {
            StringView stub_name;
            Code      *code;
        } native;
    };
} ARM64Function;

DA(ARM64Function)

typedef struct int_list {
    size_t           value;
    struct int_list *next;
} IntList;

typedef struct function_list {
    ARM64Function        *function;
    struct function_list *next;
} FunctionList;

typedef struct assembly {
    struct arm64_context *ctx;
    IRModule             *module;
    DA_ARM64Function      functions;
    ARM64Scope            scope;
    Code                 *code;
    Code                 *data;
    bool                  has_exports;
    bool                  has_main;
    StringID             *strings;
} Assembly;

DA(Assembly)

typedef struct arm64_context {
    IRProgram  *program;
    ARM64Scope  scope;
    DA_Assembly assemblies;
    IntList    *stack_depths;
    size_t      stack_allocated;
} ARM64Context;

typedef struct opcode_map {
    type_id       type;
    char const   *load_opcode;
    char const   *store_opcode;
    RegisterWidth reg_width;
} OpcodeMap;

OPTIONAL(OpcodeMap)

extern OpcodeMap             get_opcode_map(type_id type);
extern Code                 *code_create(ARM64Function *function);
extern void                  code_add_instruction(Code *code, char const *opcode, char const *arg_fmt, ...);
extern void                  code_vadd_instruction(Code *code, char const *opcode, char const *arg_fmt, va_list args);
extern void                  code_add_text(Code *code, char const *text, ...);
extern void                  code_vadd_text(Code *code, char const *text, va_list args);
extern void                  code_add_label(Code *code, StringView label);
extern void                  code_add_directive(Code *code, char const *directive, char const *args);
extern void                  code_add_export(Code *code, StringView function_name);
extern void                  code_add_import(Code *code, StringView function_name);
extern void                  code_add_comment(Code *code, char const *text, ...);
extern void                  code_vadd_comment(Code *code, char const *text, va_list args);
extern void                  code_copy_pointers(Code *code, Register to_pointer, size_t to_offset, Register from_pointer, size_t from_offset, size_t size);
extern void                  code_copy_to_registers(Code *code, Register first, Register from_pointer, size_t from_offset, size_t size);
extern void                  code_copy_from_registers(Code *code, Register to_pointer, size_t to_offset, Register r, size_t size);
extern void                  code_copy_to_stack(Code *code, Register r, size_t size);
extern void                  code_copy(Code *code, ValueLocation to_location, ValueLocation from_location);
extern void                  code_push(Code *code, Register r);
extern void                  code_pop(Code *code, Register r);
extern void                  code_append_code(Code *code, Code *append);
extern StringView            code_to_string(Code *code);
extern bool                  code_empty(Code *code);
extern bool                  code_has_text(Code *code);
extern void                  code_close_function(Code *code, StringView name, size_t stack_depth);
extern void                  code_select_prolog(Code *code);
extern void                  code_select_epilog(Code *code);
extern void                  code_select_code(Code *code);
extern Assembly             *assembly_acreate(Allocator *allocator, IRModule *module);
extern size_t                assembly_add_string(Assembly *assembly, StringView str);
extern void                  assembly_add_data(Assembly *assembly, StringView label, bool global, StringView type, bool is_static, StringView value);
extern StringView            assembly_to_string(Assembly *assembly);
extern bool                  assembly_has_exports(Assembly *assembly);
extern bool                  assembly_has_main(Assembly *assembly);
extern void                  assembly_new_function(Assembly *assembly);
extern void                  assembly_save_and_assemble(Assembly *assembly, StringView file_name);
extern ARM64Function        *assembly_function_by_name(Assembly *assembly, StringView name);
extern StringView            value_location_to_string(ValueLocation loc, Allocator *allocator);
extern StringView            arm64function_label(ARM64Function *function);
extern StringView            arm64function_to_string(ARM64Function *function);
extern ARM64Variable        *arm64function_variable_by_name(ARM64Function *function, StringView name);
extern void                  arm64function_add_instruction(ARM64Function *function, char const *opcode, char const *arg_fmt, ...);
extern void                  arm64function_add_text(ARM64Function *function, char const *text, ...);
extern void                  arm64function_vadd_text(ARM64Function *function, char const *text, va_list args);
extern void                  arm64function_add_text_sv(ARM64Function *function, StringView text);
extern void                  arm64function_add_label(ARM64Function *function, StringView label);
extern void                  arm64function_add_comment(ARM64Function *function, char const *comment, ...);
extern void                  arm64function_syscall(ARM64Function *function, int id);
extern void                  arm64function_syscall1(ARM64Function *function, int id, uint64_t arg1);
extern void                  arm64function_syscall2(ARM64Function *function, int id, uint64_t arg1, uint64_t arg2);
extern void                  arm64function_syscall3(ARM64Function *function, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3);
extern void                  arm64function_syscall4(ARM64Function *function, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3, uint64_t arg4);
extern void                  arm64function_append_code(ARM64Function *function, Code *code);
extern void                  arm64function_push(ARM64Function *function, Register r);
extern void                  arm64function_pop(ARM64Function *function, Register r);
extern void                  arm64function_copy_pointers(ARM64Function *function, Register to_pointer, size_t to_offset, Register from_pointer, size_t from_offset, size_t size);
extern void                  arm64function_copy_to_registers(ARM64Function *function, Register first, Register from_pointer, size_t from_offset, size_t size);
extern void                  arm64function_copy_from_registers(ARM64Function *function, Register to_pointer, size_t to_offset, Register r, size_t size);
extern void                  arm64function_copy_to_stack(ARM64Function *function, Register r, size_t size);
extern void                  arm64function_copy(ARM64Function *function, ValueLocation to_location, ValueLocation from_location);
extern void                  arm64function_write_char(ARM64Function *function, int fd, char ch);
extern Register              arm64function_allocate_register(ARM64Function *function);
extern void                  arm64function_release_register(ARM64Function *function, Register reg);
extern void                  arm64function_release_all_registers(ARM64Function *function);
extern OptionalValueLocation arm64function_pop_location(ARM64Function *function);
extern void                  arm64function_push_location(ARM64Function *function, ValueLocation entry);
extern void                  arm64function_push_register(ARM64Function *function, type_id type, Register reg);
extern void                  arm64function_enter(ARM64Function *func);
extern void                  arm64function_return(ARM64Function *func);
extern void                  arm64function_leave(ARM64Function *func);
extern void                  arm64function_marshall_arguments(ARM64Function *function);
extern void                  arm64function_marshall_return(ARM64Function *function, bool discard_result);
extern StringView            arm64variable_to_string(ARM64Variable *var);
extern void                  arm64variable_store_variable(ARM64Variable *variable, ValueLocation from_location);
extern void                  arm64variable_load_variable(ARM64Variable *variable, ValueLocation to_location);
extern ARM64Function        *arm64context_function_by_name(ARM64Context *ctx, StringView name);
extern ARM64Context         *generate_arm64(IRProgram *program);
extern ErrorOrInt            output_arm64(IRProgram *program);

static inline size_t align_at(size_t value, size_t alignment)
{
    if (value % alignment) {
        value += alignment - (value % alignment);
    }
    return value;
}

#endif /* __ARM64_H__ */
