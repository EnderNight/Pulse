    .text


# void sys_write(int, void*, unsigned long)
    .globl  sys_write
    .type   sys_write, @function
sys_write:
    mov     $1, %rax
    syscall

    ret

    .size   sys_write,.-sys_write


# void sys_exit(int)
    .globl  sys_exit
    .type   sys_exit, @function
sys_exit:
    mov     $60, %rax
    syscall

    ret

    .size   sys_exit,.-sys_exit


.section .note.GNU-stack,"",@progbits
