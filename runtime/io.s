    .text

# void print(long)
    .globl  print
    .type   print, @function
print:
    push    %rbp
    mov     %rsp, %rbp
    sub     $16, %rsp

    mov     %rdi, -4(%rbp)
    lea     -4(%rbp), %rsi
    mov     $1, %rdi
    mov     $1, %rdx
    call    sys_write

    leave
    ret

    .size   print,.-print


# void print_int(unsigned long)
    .globl  print_int
    .type   print_int, @function
print_int:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $64, %rsp
    movq    %rdi, -56(%rbp)
    movq    $0, -48(%rbp)
    movq    $0, -40(%rbp)
    movq    $0, -32(%rbp)
    movq    $0, -24(%rbp)
    movq    $0, -8(%rbp)
.L2:
    movq    -56(%rbp), %rcx
    movabsq $-3689348814741910323, %rdx
    movq    %rcx, %rax
    mulq    %rdx
    shrq    $3, %rdx
    movq    %rdx, %rax
    salq    $2, %rax
    addq    %rdx, %rax
    addq    %rax, %rax
    subq    %rax, %rcx
    movq    %rcx, %rdx
    movl    %edx, %eax
    leal    48(%rax), %edx
    movl    $31, %eax
    subq    -8(%rbp), %rax
    movb    %dl, -48(%rbp,%rax)
    addq    $1, -8(%rbp)
    movq    -56(%rbp), %rax
    movabsq $-3689348814741910323, %rdx
    mulq    %rdx
    movq    %rdx, %rax
    shrq    $3, %rax
    movq    %rax, -56(%rbp)
    cmpq    $0, -56(%rbp)
    jne     .L2
    movl    $32, %eax
    subq    -8(%rbp), %rax
    leaq    -48(%rbp), %rdx
    leaq    (%rdx,%rax), %rcx
    movq    -8(%rbp), %rax
    movq    %rax, %rdx
    movq    %rcx, %rsi
    movl    $1, %edi
    call    sys_write
    nop
    leave
    ret

    .size    print_int, .-print_int



.section .note.GNU-stack,"",@progbits
