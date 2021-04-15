.data
format:
    .asciz  "%llu \n"
.text
.global _main

_main:
    push    %rbp
    mov     %rsp, %rbp
    mov     $46, %rdi               # param 1 to fib
    call    _fib                     # calling fib
    lea     format(%rip), %rdi
    mov     %rax, %rsi
    xor     %rax, %rax
    call    _printf
    xor     %rax, %rax
    pop     %rbp
    ret
_fib:
    push    %rbp
    mov     %rsp, %rbp
    push    %r14
    push    %rbx
    mov     $1, %r14
    cmp     $2, %rdi
    jb      _fib_below_2
    mov     %rdi, %rbx
_fib_2:
    lea     -1(%rbx), %rdi
    call    _fib
    add     $-2, %rbx
    add     %rax, %r14
    cmp     $1, %rbx
    ja      _fib_2
_fib_below_2:
    mov     %r14, %rax
    pop     %rbx
    pop     %r14
    pop     %rbp
    ret
