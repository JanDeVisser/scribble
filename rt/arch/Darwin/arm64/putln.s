.align 4

.global scribble$putln

//
// putln - Print string followed by a newline character
//
// In:
len     .req w0     // Length of the string
buffer  .req x1     // Pointer to the string buffer

// Out:
//   x0: Number of characters printed.

// Work:
//   x7 - characters printed
//   x16 - syscall

scribble$putln:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp

    bl      scribble$puts
    mov     x7,x0
    bl      scribble$endln
    add     x0,x7,x0
    ldp     fp,lr,[sp],#16
    ret
