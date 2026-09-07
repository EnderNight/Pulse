# Pulse

Simple programming language.

# Usage

`pulse <file>` then exec `./main.pulse.exe`

## Sys V ABI conventions

; Args order: rdi, rsi, rdx, rcx, r8, r9, stack...
; Caller-saved (can use freely in function):
;   rax, rcx, rdx, rsi, rdi, r8-11, xmm0-15
; Callee-saved (must save before use in function):
;   rbx, rsp, rbp, r12-15
; rsp must be aligned to 16 bytes before any fn/syscall calls

