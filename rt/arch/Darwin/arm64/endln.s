.align 4

.global scribble$endln

//
// endln - Prints a newline character
//
// In:

// Out:
//   x0: Number of characters printed.

// Work:
//   x16 - syscall

scribble$endln:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp

    mov     x0,1
    adr     x1,__str_newline
    mov     x2,#1
    mov     x16,#0x04
    svc     #0x00
    ldp     fp,lr,[sp],#16
    ret

__str_newline:
    .string "\n"
