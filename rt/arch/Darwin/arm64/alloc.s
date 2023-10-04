.include "arch/Darwin/arm64/syscalls.inc"
.align 4

alloc:
    mov     x1,x0
    mov     x0,xzr
    mov     w2,#3
    mov     w3,#0x1002
    mov     w4,#-1
    mov     x5,xzr
    mov     x16,syscall_mmap
    svc     #0x00
