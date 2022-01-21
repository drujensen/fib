.data
format:
    .asciz  "%llu \n"
.text
.global main

fib:
    push    %rbp
    mov     %rsp, %rbp
    push    %r14
    push    %rbx
    mov     %rdi, %r14
    cmp     $2, %rdi
    jb      fib_below_2
    mov     %rdi, %rbx
fib_2:
    lea     -1(%rbx), %rdi
    call    fib
    add     $-2, %rbx
    add     %rax, %r14
    cmp     $1, %rbx
    ja      fib_2
fib_below_2:
    mov     %r14, %rax
    pop     %rbx
    pop     %r14
    pop     %rbp
    ret
main:
    push    %rbp
    mov     %rsp, %rbp
    mov     $47, %rdi               # param 1 to fib
    call    fib                     # calling fib
    lea     format(%rip), %rdi
    mov     %rax, %rsi
    xor     %rax, %rax
    call    printf
    xor     %rax, %rax
    pop     %rbp
    ret
