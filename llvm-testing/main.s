	.text
	.file	"global_mod"
	.globl	sum                             # -- Begin function sum
	.p2align	4, 0x90
	.type	sum,@function
sum:                                    # @sum
	.cfi_startproc
# %bb.0:                                # %entry
                                        # kill: def $esi killed $esi def $rsi
                                        # kill: def $edi killed $edi def $rdi
	movl	%edi, -8(%rsp)
	movl	%esi, -4(%rsp)
	leal	(%rdi,%rsi), %eax
	retq
.Lfunc_end0:
	.size	sum, .Lfunc_end0-sum
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movl	$0, 12(%rsp)
	leaq	.L__unnamed_1(%rip), %rbx
	cmpl	$9, 12(%rsp)
	jg	.LBB1_3
	.p2align	4, 0x90
.LBB1_2:                                # %loop_body
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rsp), %esi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	incl	12(%rsp)
	cmpl	$9, 12(%rsp)
	jle	.LBB1_2
.LBB1_3:                                # %loop_end
	xorl	%eax, %eax
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
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
