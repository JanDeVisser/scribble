/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <limits.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <datum.h>
#include <log.h>
#include <native.h>
#include <type.h>

typedef struct trampoline {
    void_t   fnc;
    uint64_t x[8];
    double   d[8];
    uint64_t int_return_value;
    double   double_return_value;
} Trampoline;

void native_call(StringView name, size_t argc, Datum **values, Datum *ret)
{
    if (argc > 8) {
        fatal("Can't do native calls with more than 8 parameters");
    }
    Trampoline t = { 0 };
    t.fnc = resolve_function(sv_cstr(name));
    if (!t.fnc) {
        fatal("Function '%.*s' not found", SV_ARG(name));
    }

    // Stage A - Initialization
    // This stage is performed exactly once, before processing of the arguments
    // commences.

    // A.1 The Next General-purpose Register Number (NGRN) is set to zero.
    size_t ngrn = 0;
    // A.2 The Next SIMD and Floating-point Register Number (NSRN) is set to
    // zero.
    size_t nsrn = 0;
    // A.3 The Next Scalable Predicate Register Number (NPRN) is set to zero.
    size_t nprn = 0;
    // A.4 The next stacked argument address (NSAA) is set to the current
    // stack-pointer value (SP).
    size_t nsaa = 0;

    for (size_t ix = 0; ix < argc; ++ix) {
        ExpressionType *et = type_registry_get_type_by_id(values[ix]->type);

        // Stage B – Pre-padding and extension of arguments
        // For each argument in the list the first matching rule from the
        // following list is applied. If no rule matches the argument is used
        // unmodified.

        // B.1 If the argument type is a Pure Scalable Type, no change is made
        // at this stage.

        // B.2 If the argument type is a Composite Type whose size cannot be
        // statically determined by both the caller and the callee, the
        // argument is copied to memory and the argument is replaced by a
        // pointer to the copy. (There are no such types in C/C++ but they
        // exist in other languages or in language extensions).

        // B.3 If the argument type is an HFA or an HVA, then the argument is
        // used unmodified.
        // TODO

        // B.4 If the argument type is a Composite Type that is larger than 16
        // bytes, then the argument is copied to memory allocated by the caller
        // and the argument is replaced by a pointer to the copy.
        // TODO

        // B.5 If the argument type is a Composite Type then the size of the
        // argument is rounded up to the nearest multiple of 8 bytes.

        // B.6 If the argument is an alignment adjusted type its value is passed
        // as a copy of the actual value. The copy will have an alignment
        // defined as follows:
        //     • For a Fundamental Data Type, the alignment is the natural
        //       alignment of that type, after any promotions.
        //     • For a Composite Type, the alignment of the copy will have
        //       8-byte alignment if its natural alignment is ≤ 8 and 16-byte
        //       alignment if its natural alignment is ≥ 16.
        // The alignment of the copy is used for applying marshaling rules.

        // Stage C – Assignment of arguments to registers and stack
        // For each argument in the list the following rules are applied in
        // turn until the argument has been allocated. When an argument is
        // assigned to a register any unused bits in the register have
        // unspecified value. When an argument is assigned to a stack slot any
        // unused padding bytes have unspecified value.

        // C.1 If the argument is a Half-, Single-, Double- or Quad- precision
        // Floating-point or short vector type and the NSRN is less than 8, then
        // the argument is allocated to the least significant bits of register
        // v[NSRN]. The NSRN is incremented by one. The argument has now been
        // allocated.
        if ((et->type_id == FLOAT_ID) && (nsrn < 8)) {
            t.d[nsrn] = values[ix]->float_value;
            ++nsrn;
            continue;
        }

        // C.2 If the argument is an HFA or an HVA and there are sufficient
        // unallocated SIMD and Floating-point registers (NSRN + number of
        // members ≤ 8), then the argument is allocated to SIMD and
        // Floating-point registers (with one register per member of the HFA or
        // HVA). The NSRN is incremented by the number of registers used. The
        // argument has now been allocated.
        // TODO

        // C.3 If the argument is an HFA or an HVA then the NSRN is set to 8 and
        // the size of the argument is rounded up to the nearest multiple of 8
        // bytes.
        // TODO

        // C.4 If the argument is an HFA, an HVA, a Quad-precision
        // Floating-point or short vector type then the NSAA is rounded up to
        // the next multiple of 8 if its natural alignment is ≤ 8 or the next
        // multiple of 16 if its natural alignment is ≥ 16.
        // TODO

        // C.5 If the argument is a Half- or Single- precision Floating Point
        // type, then the size of the argument is set to 8 bytes. The effect is
        // as if the argument had been copied to the least significant bits of a
        // 64-bit register and the remaining bits filled with unspecified
        // values.
        // Not supported

        // C.6 If the argument is an HFA, an HVA, a Half-, Single-, Double- or
        // Quad- precision Floating-point or short vector type, then the
        // argument is copied to memory at the adjusted NSAA. The NSAA is
        // incremented by the size of the argument. The argument has now been
        // allocated.
        // TODO

        // C.7 If the argument is a Pure Scalable Type that consists of NV
        // Scalable Vector Types and NP Scalable Predicate Types, if the
        // argument is named, if NSRN+NV ≤ 8, and if NPRN+NP ≤ 4, then the
        // Scalable Vector Types are allocated in order to
        // z[NSRN]...z[NSRN+NV-1] inclusive and the Scalable Predicate Types are
        // allocated in order to p[NPRN]...p[NPRN+NP-1] inclusive. The NSRN is
        // incremented by NV and the NPRN is incremented by NP. The argument has
        // now been allocated.
        // TODO

        // C.8 If the argument is a Pure Scalable Type that has not been
        // allocated by the rules above, then the argument is copied to memory
        // allocated by the caller and the argument is replaced by a pointer to
        // the copy (as for B.4 above). The argument is then allocated according
        // to the rules below.
        // TODO

        // C.9 If the argument is an Integral or Pointer Type, the size of the
        // argument is less than or equal to 8 bytes and the NGRN is less than
        // 8, the argument is copied to the least significant bits in x[NGRN].
        // The NGRN is incremented by one. The argument has now been allocated.
        if ((type_kind(et) == TK_PRIMITIVE) && (ngrn < 8)) {
            BuiltinType builtin_type = typeid_builtin_type(et->type_id);
            if (BuiltinType_is_integer(builtin_type) || builtin_type == BIT_POINTER) {
                t.x[ngrn] = datum_unsigned_integer_value(values[ix]);
                ++ngrn;
                continue;
            }
        }

        // C.10 If the argument has an alignment of 16 then the NGRN is rounded
        // up to the next even number.

        // C.11 If the argument is an Integral Type, the size of the argument
        // is equal to 16 and the NGRN is less than 7, the argument is copied to
        // x[NGRN] and x[NGRN+1]. x[NGRN] shall contain the lower addressed
        // double-word of the memory representation of the argument. The NGRN
        // is incremented by two. The argument has now been allocated.

        // C.12 If the argument is a Composite Type and the size in double-words
        // of the argument is not more than 8 minus NGRN, then the argument is
        // copied into consecutive general-purpose registers, starting at
        // x[NGRN]. The argument is passed as though it had been loaded into the
        // registers from a double-word-aligned address with an appropriate
        // sequence of LDR instructions loading consecutive registers from
        // memory (the contents of any unused parts of the registers are
        // unspecified by this standard). The NGRN is incremented by the number
        // of registers used. The argument has now been allocated.

        // C.13 The NGRN is set to 8.

        // C.14 The NSAA is rounded up to the larger of 8 or the Natural
        // Alignment of the argument’s type.

        // C.15 If the argument is a composite type then the argument is copied
        // to memory at the adjusted NSAA. The NSAA is incremented by the size
        // of the argument. The argument has now been allocated.

        // C.16 If the size of the argument is less than 8 bytes then the size
        // of the argument is set to 8 bytes. The effect is as if the argument
        // was copied to the least significant bits of a 64-bit register and the
        // remaining bits filled with unspecified values.

        // C.17 The argument is copied to memory at the adjusted NSAA. The NSAA
        // is incremented by the size of the argument. The argument has now been
        // allocated.
    }

    int trampoline_result = trampoline(&t);
    if (trampoline_result) {
        fatal("Error executing '%.*s'. Trampoline returned %d", SV_ARG(name), trampoline_result);
    }
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        ret->integer.n = (ct) t.int_return_value;       \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case BIT_BOOL:
        ret->bool_value = (bool) t.int_return_value;
        break;
    case BIT_POINTER:
        ret->pointer.ptr = (void *) t.int_return_value;
        break;
    case BIT_FLOAT:
        ret->float_value = t.double_return_value;
        break;
    case BIT_VOID:
        ret->void_value = 0;
        break;
    default:
        UNREACHABLE();
    }
}
