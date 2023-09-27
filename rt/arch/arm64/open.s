.align 4
.global scribble$open
.global _scribble$open

;
; puthex - Print integer in hexadecimal.
;
; In:
name    .req x0     ; Pointer to filename
len     .req x1     ; Filename length
mode    .req x2     ; File open mode

; Out:
;   x0: File handle

; Work:
;   ---

scribble$open:
_scribble$open:
    stp         fp,lr,[sp,#-16]!
    mov         fp,sp
    add         x3,name,len
    ldrb        w3,[x3]
    cmp         x3,#0x00
    b.eq        __open_name_is_cstr
    add         len,len,#0x01
    mov         x3,len
    and         x4,x3,0x0F
    cbz         x4, __open_len_mod_16_zero
    add         x3,x3,#0x10
    bic         x3,x3,#0x0F
__open_len_mod_16_zero:
    sub         sp,sp,x3
    mov         x4,sp
    sub         x3,len,#0x01 ; Start loop at 1 because len is one more than the
                            ; actual length of the string.
__open_copy_to_stack_loop:
    cbz         x3,__open_copied_name_to_stack
    ldrb        w5,[x0],0x01
    strb        w5,[x4],0x01
    sub         x3,x3,#0x01
    b           __open_copy_to_stack_loop
__open_copied_name_to_stack:
    mov         w5,wzr
    strb        w5,[x4]
    mov         x0,sp
 __open_name_is_cstr:
    mov         mode,x1
    mov         x16,#0x0005
    svc         #0x00
    mov         sp,fp
    ldp         fp,lr,[sp],#16
    ret
