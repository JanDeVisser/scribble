/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.global _trampoline

.align 4

_trampoline:
        stp     fp, lr, [sp, #-16]! ; Set up SP, FP, LR
        mov     fp, sp
        mov     x12, x0             ; Get *Trampoline

        ldr     x0, [x12, 8]        ; Load general purpose registers
        ldr     x1, [x12, 16]
        ldr     x2, [x12, 24]
        ldr     x3, [x12, 32]
        ldr     x4, [x12, 40]
        ldr     x5, [x12, 48]
        ldr     x6, [x12, 56]
        ldr     x7, [x12, 64]

        ldr     d0, [x12, 72]       ; Load FP registers
        ldr     d1, [x12, 80]
        ldr     d2, [x12, 88]
        ldr     d3, [x12, 96]
        ldr     d4, [x12, 104]
        ldr     d5, [x12, 112]
        ldr     d6, [x12, 120]
        ldr     d7, [x12, 128]

        str     x12, [sp, #-16]!    ; Save x12 (caller saved)
        ldr     x16, [x12]          ; Load function pointer in x16
        blr     x16                 ; Call function pointer
        ldr     x12, [sp], #16      ; Restore x12
        str     x0, [x12, 136]      ; Store x0 to int_return_value
        str     d0, [x12, 144]      ; Store d0 to float_return_value
        mov     x0, xzr             ; Return all good
        mov     sp, fp              ; Restore SP, FP, and LR
        ldp     fp, lr, [sp], 16
        ret                         ; Return
