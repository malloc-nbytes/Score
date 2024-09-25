; ModuleID = 'global_mod'
source_filename = "global_mod"

; Function Attrs: nounwind
declare i32 @printf(ptr, ...) #0

; Function Attrs: nounwind
declare ptr @malloc(i32) #0

; Function Attrs: nounwind
declare void @free(ptr) #0

define i32 @sum(i32 %0, i32 %1) {
entry:
  %b = alloca i32, align 4
  %a = alloca i32, align 4
  store i32 %0, ptr %a, align 4
  store i32 %1, ptr %b, align 4
  %a1 = load i32, ptr %a, align 4
  %b2 = load i32, ptr %b, align 4
  %addtmp = add i32 %a1, %b2
  ret i32 %addtmp
}

define i32 @main() {
entry:
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %loop_cond

loop_cond:                                        ; preds = %loop_body, %entry
  %i1 = load i32, ptr %i, align 4
  %cmptmp = icmp slt i32 %i1, 10
  br i1 %cmptmp, label %loop_body, label %loop_end

loop_body:                                        ; preds = %loop_cond
  %i2 = load i32, ptr %i, align 4
  %addtmp = add i32 %i2, 1
  store i32 %addtmp, ptr %i, align 4
  br label %loop_cond

loop_end:                                         ; preds = %loop_cond
  %x = alloca ptr, align 8
  %0 = call ptr @malloc(i32 1000000)
  store ptr %0, ptr %x, align 8
  %x3 = load ptr, ptr %x, align 8
  call void @free(ptr %x3)
  ret i32 0
}

attributes #0 = { nounwind }

