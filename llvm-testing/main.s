	.text
	.file	"global_mod"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$16, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movl	$40, %edi
	callq	malloc@PLT
	movq	%rax, -32(%rbp)
	movl	$0, -20(%rbp)
	cmpl	$9, -20(%rbp)
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %loop_body
                                        # =>This Inner Loop Header: Depth=1
	movq	-32(%rbp), %rax
	movslq	-20(%rbp), %rcx
	movl	%ecx, (%rax,%rcx,8)
	incl	-20(%rbp)
	cmpl	$9, -20(%rbp)
	jle	.LBB0_2
.LBB0_3:                                # %loop_end
	movq	%rsp, %rax
	leaq	-16(%rax), %r14
	movq	%r14, %rsp
	movl	$0, -16(%rax)
	leaq	.L__unnamed_1(%rip), %rbx
	cmpl	$9, (%r14)
	jg	.LBB0_6
	.p2align	4, 0x90
.LBB0_5:                                # %loop_body8
                                        # =>This Inner Loop Header: Depth=1
	movq	-32(%rbp), %rax
	movslq	(%r14), %rcx
	pinsrw	$0, (%rax,%rcx,4), %xmm0
	movq	%rbx, %rdi
	movb	$1, %al
	callq	printf@PLT
	incl	(%r14)
	cmpl	$9, (%r14)
	jle	.LBB0_5
.LBB0_6:                                # %loop_end9
	movq	-32(%rbp), %rdi
	callq	free@PLT
	xorl	%eax, %eax
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L__unnamed_1,@object           # @0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"%d\n"
	.size	.L__unnamed_1, 4

	.section	".note.GNU-stack","",@progbits
