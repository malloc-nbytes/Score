	.text
	.file	"global_mod"
	.globl	test                            # -- Begin function test
	.p2align	4, 0x90
	.type	test,@function
test:                                   # @test
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, 8(%rsp)
	movl	$0, 4(%rsp)
	leaq	.L__unnamed_1(%rip), %rbx
	cmpl	$10, 4(%rsp)
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %loop_body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rax
	movslq	4(%rsp), %rcx
	movl	(%rax,%rcx,4), %esi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	incl	4(%rsp)
	cmpl	$10, 4(%rsp)
	jle	.LBB0_2
.LBB0_3:                                # %loop_end
	xorl	%eax, %eax
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	test, .Lfunc_end0-test
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	$44, %edi
	callq	malloc@PLT
	movq	%rax, 16(%rsp)
	movl	$0, 12(%rsp)
	cmpl	$10, 12(%rsp)
	jg	.LBB1_3
	.p2align	4, 0x90
.LBB1_2:                                # %loop_body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rsp), %rax
	movslq	12(%rsp), %rcx
	movl	%ecx, (%rax,%rcx,4)
	incl	12(%rsp)
	cmpl	$10, 12(%rsp)
	jle	.LBB1_2
.LBB1_3:                                # %loop_end
	movq	16(%rsp), %rdi
	callq	test@PLT
	movq	16(%rsp), %rdi
	callq	free@PLT
	xorl	%eax, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	.L__unnamed_1,@object           # @0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"%d\n"
	.size	.L__unnamed_1, 4

	.section	".note.GNU-stack","",@progbits
