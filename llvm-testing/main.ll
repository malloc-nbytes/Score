; ModuleID = 'global_mod'
source_filename = "global_mod"

@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: nounwind
declare i32 @printf(ptr, ...) #0

; Function Attrs: nounwind
declare ptr @malloc(i64) #0

; Function Attrs: nounwind
declare void @free(ptr) #0

define i32 @main() {
entry:
  %arr = alloca ptr, align 8
  %0 = call ptr @malloc(i64 40)
  store ptr %0, ptr %arr, align 8
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %loop_cond

loop_cond:                                        ; preds = %loop_body, %entry
  %i1 = load i32, ptr %i, align 4
  %cmptmp = icmp slt i32 %i1, 10
  br i1 %cmptmp, label %loop_body, label %loop_end

loop_body:                                        ; preds = %loop_cond
  %arr2 = load ptr, ptr %arr, align 8
  %i3 = load i32, ptr %i, align 4
  %indexptr = getelementptr inbounds i32, ptr %arr2, i32 %i3
  %i4 = load i32, ptr %i, align 4
  store i32 %i4, ptr %indexptr, align 4
  %i5 = load i32, ptr %i, align 4
  %addtmp = add i32 %i5, 1
  store i32 %addtmp, ptr %i, align 4
  br label %loop_cond

loop_end:                                         ; preds = %loop_cond
  %i6 = alloca i32, align 4
  store i32 0, ptr %i6, align 4
  br label %loop_cond7

loop_cond7:                                       ; preds = %loop_body8, %loop_end
  %i10 = load i32, ptr %i6, align 4
  %cmptmp11 = icmp slt i32 %i10, 10
  br i1 %cmptmp11, label %loop_body8, label %loop_end9

loop_body8:                                       ; preds = %loop_cond7
  %arr12 = load ptr, ptr %arr, align 8
  %i13 = load i32, ptr %i6, align 4
  %indexptr14 = getelementptr inbounds i32, ptr %arr12, i32 %i13
  %loadtmp = load i32, ptr %indexptr14, align 4
  %1 = call i32 (ptr, ...) @printf(ptr @0, i32 %loadtmp)
  %i15 = load i32, ptr %i6, align 4
  %addtmp16 = add i32 %i15, 1
  store i32 %addtmp16, ptr %i6, align 4
  br label %loop_cond7

loop_end9:                                        ; preds = %loop_cond7
  %arr17 = load ptr, ptr %arr, align 8
  call void @free(ptr %arr17)
  ret i32 0
}

attributes #0 = { nounwind }

