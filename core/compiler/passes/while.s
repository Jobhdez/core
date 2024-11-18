	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $0, -8(%rbp)
	movq $0, -16(%rbp)
loop_1:
	movq -16(%rbp), %rax
	addq %rax, -8(%rbp)
	movq $1, %rax
	addq %rax, -16(%rbp)
	cmpq $5, -16(%rbp)
	jl loop_1
	movq -8(%rbp), %rdi
	callq print_int

conclusion:
	addq $16, %rsp
	popq %rbp
	retq