/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.global _trampoline

.align 4

marshall_param:
        stp     x29, x30, [sp, -48]!
        mov     x29, sp

        str     x0,[sp, 16]  ; Save *value
        ldr     x2,[x0]      ; Load value->type in x2

        and     x1,x2,#0x0600
        cmp     x1,xzr
        b.eq    __type_other ; Type is not an integer. We'll deal with that later

        and     x1,x2,#0xFF  ; Width is type & 0x00FF

        cmp     x1,#8
        b.gt    __type_16
        ldr     b0,[x0,#8]
        b       __marshall_param_done

__type_16:
        cmp     x1,#16
        b.gt    __type_32
        ldr     h0,[x0,#8]
        b       __marshall_param_done

__type_32:
        cmp     x1,#32
        b.gt    __type_64
        ldr     w0,[x0,#8]
        b       __marshall_param_done

__type_64:
        cmp     x1,#64
        b.gt    __type_other
        ldr     x0,[x0,#8]
        b       __marshall_param_done

__type_other:
        mov     x0, x2 ; xzr

__marshall_param_done:
        mov     sp, x29
        ldp     x29, x30, [sp],48
        ret

marshall_retval:
        stp     x29, x30, [sp, -48]!
        mov     x29, sp

        stp     x1,x0,[sp, 16]  ; Save *retval (16), return value (24)
        ldr     x2,[x1]         ; Load retval->type in x2

        and     x3,x2,#0x0600
        cmp     x3,xzr
        b.eq    __retval_type_other ; Type is not an integer. We'll deal with that later

        and     x3,x2,#0xFF  ; Width is type & 0x00FF

        cmp     x3,#8
        b.gt    __retval_type_16
        str     b0,[x1,#8]
        b       __marshall_retval_done

__retval_type_16:
        cmp     x3,#16
        b.gt    __retval_type_32
        str     h0,[x1,#8]
        b       __marshall_retval_done

__retval_type_32:
        cmp     x3,#32
        b.gt    __retval_type_64
        str     w0,[x1,#8]
        b       __marshall_retval_done

__retval_type_64:
        cmp     x3,#64
        b.gt    __retval_type_other
        str     x0,[x1,#8]
        b       __marshall_retval_done

__retval_type_other:
        str     xzr,[x1,#8]

__marshall_retval_done:
        mov     sp, x29
        ldp     x29, x30, [sp],48
        ret

_trampoline:
        stp     x29, x30, [sp, -48]!
        mov     x29, sp
        stp     x1,x0,[sp, 32]  ; Save argc (32), function pointer (40)
        stp     x3,x2,[sp, 16]  ; Save *ret (16), **values (24)
        mov     x12, x2         ; Get **values
        mov     x14, x1         ; Get argc

        ldr     x12, [sp, 24]   ; **values in x12
        cmp     x14, #1
        b.lt    __trampoline_call
        ldr     x0, [x12], 8    ; *values[0] in x0, x12 now points to *values[1]
        bl      marshall_param

        cmp     x14, #2
        b.lt    __trampoline_call
        str     x0,[sp,-16]!
        ldr     x0, [x12], 8
        bl      marshall_param
        str     x0, [sp], 16

        cmp     x14, #3
        b.lt    __load_x1
        ldr     x0, [x12], 8
        bl      marshall_param
        str     x0, [sp], 16

        cmp     x14, #4
        b.lt    __load_x2
        ldr     x0, [x12], 8
        bl      marshall_param
        str     x0, [sp], 16

        cmp     x14, #5
        b.lt    __load_x3
        ldr     x0, [x12], 8
        bl      marshall_param
        str     x0, [sp], 16

        cmp     x14, #6
        b.lt    __load_x4
        ldr     x0, [x12], 8
        bl      marshall_param
        str     x0, [sp], 16

        cmp     x14, #7
        b.lt    __load_x5
        ldr     x0, [x12], 8
        bl      marshall_param
        str     x0, [sp], 16

        cmp     x14, #8
        b.lt    __load_x6
        ldr     x0, [x12], 8
        bl      marshall_param
        mov     x7, x0

__load_x6:
        ldr     x6, [sp], 16

__load_x5:
        ldr     x5, [sp], 16

__load_x4:
        ldr     x4, [sp], 16

__load_x3:
        ldr     x3, [sp], 16

__load_x2:
        ldr     x2, [sp], 16

__load_x1:
        ldr     x1, [sp], 16
        ldr     x0, [sp], 16

__trampoline_call:
        ldr     x16,[sp, 40]
        blr     x16
        ldr     x1, [sp, 16]
        bl      marshall_retval
        mov     x0, xzr

        mov     sp, x29
        ldp     x29, x30, [sp],48
        ret
