.global _start

.include "arch/Darwin/arm64/syscalls.inc"

.align 2
_start:
    mov     fp,sp

    # bl      static_initializer
    bl      main
    mov     x16, syscall_exit
    svc     0
