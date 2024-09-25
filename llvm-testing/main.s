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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	pushq	%rax
	.cfi_offset %rbx, -24
	movl	$0, -12(%rbp)
	cmpl	$9, -12(%rbp)
	jg	.LBB1_3
	.p2align	4, 0x90
.LBB1_2:                                # %loop_body
                                        # =>This Inner Loop Header: Depth=1
	incl	-12(%rbp)
	cmpl	$9, -12(%rbp)
	jle	.LBB1_2
.LBB1_3:                                # %loop_end
	movq	%rsp, %rbx
	leaq	-16(%rbx), %rsp
	movl	$1000000, %edi                  # imm = 0xF4240
	callq	malloc@PLT
	movq	%rax, -16(%rbx)
	movq	%rax, %rdi
	callq	free@PLT
	xorl	%eax, %eax
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
