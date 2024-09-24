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
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$5, %edi
	movl	$5, %esi
	xorl	%eax, %eax
	callq	sum@PLT
	movl	$10, %edi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	sum@PLT
	movl	%eax, 4(%rsp)
	leaq	.L__unnamed_1(%rip), %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	printf@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	.L__unnamed_1,@object           # @0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"sum: %d"
	.size	.L__unnamed_1, 8

	.section	".note.GNU-stack","",@progbits
