.global _start

.include "arch/Darwin/arm64/syscalls.inc"

argc    .req w11
argv    .req x12
ix      .req w13
ptr     .req x14
str     .req x15
len     .req x0

.align 2
_start:
    mov     fp,sp

    mov     argc,w0
    mov     argv,x1

    mov     ptr,xzr
    mov     len,xzr
    stp     ptr,len,[sp,#-16]!
    cbz     argc,done

    add     str,argv,x11,lsl 3
 loop:
    ldr     ptr,[str,#-8]!
    mov     x0,ptr
    bl      scribble$strlen
    stp     ptr,len,[sp,#-16]!
    cmp     str,argv
    b.ne    loop

 done:
    mov     ptr,xzr
    mov     len,xzr
    stp     ptr,len,[sp,#-16]!

    # bl      static_initializer

    mov     w0,argc
    mov     x1,sp
    bl      main

    mov     x16, syscall_exit
    svc     0
