; ModuleID = 'global_mod'
source_filename = "global_mod"

@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: nounwind
declare i32 @printf(ptr, ...) #0

; Function Attrs: nounwind
declare ptr @malloc(i32) #0

; Function Attrs: nounwind
declare void @free(ptr) #0

define i32 @test(ptr %0) {
entry:
  %arr = alloca ptr, align 8
  store ptr %0, ptr %arr, align 8
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %loop_cond

loop_cond:                                        ; preds = %loop_body, %entry
  %i1 = load i32, ptr %i, align 4
  %cmptmp = icmp slt i32 %i1, 11
  br i1 %cmptmp, label %loop_body, label %loop_end

loop_body:                                        ; preds = %loop_cond
  %arr2 = load ptr, ptr %arr, align 8
  %i3 = load i32, ptr %i, align 4
  %indexptr = getelementptr inbounds i32, ptr %arr2, i32 %i3
  %loadtmp = load i32, ptr %indexptr, align 4
  %1 = call i32 (ptr, ...) @printf(ptr @0, i32 %loadtmp)
  %i4 = load i32, ptr %i, align 4
  %addtmp = add i32 %i4, 1
  store i32 %addtmp, ptr %i, align 4
  br label %loop_cond

loop_end:                                         ; preds = %loop_cond
  ret i32 0
}

define i32 @main() {
entry:
  %arr = alloca ptr, align 8
  %0 = call ptr @malloc(i32 44)
  store ptr %0, ptr %arr, align 8
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %loop_cond

loop_cond:                                        ; preds = %loop_body, %entry
  %i1 = load i32, ptr %i, align 4
  %cmptmp = icmp slt i32 %i1, 11
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
  %arr6 = load ptr, ptr %arr, align 8
  %1 = call i32 @test(ptr %arr6)
  %arr7 = load ptr, ptr %arr, align 8
  call void @free(ptr %arr7)
  ret i32 0
}

attributes #0 = { nounwind }

