.data
format:
    .asciz  "%llu \n"
.text
.global main

fib:
    mov     $1, %rax                # return 1
    cmp     $1, %rdi                # compare param1 to 1
    ja     fib_greater_1
    ret
fib_greater_1:
    lea     -2(%rdi), %r8
    push    %rdi
    mov     %r8, %rdi
    call    fib
    mov     %rax, %r9
    pop     %rdi
    push    %r9
    dec     %rdi
    call    fib
    pop     %r9
    add     %r9, %rax
    ret
    nop
main:
    push    %rbx                    # pushing register to align the stack
    mov     $46, %rdi               # param 1 to fib
    call    fib                     # calling fib
    lea     format(%rip), %rdi
    mov     %rax, %rsi
    xor     %rax, %rax
    call    printf
    mov     $0, %rax
    pop     %rbx
    ret
