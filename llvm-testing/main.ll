; ModuleID = 'global_mod'
source_filename = "global_mod"

@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

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
  %x = alloca i32, align 4
  store i32 0, ptr %x, align 4
  br label %loop_cond

loop_cond:                                        ; preds = %loop_body, %entry
  %x1 = load i32, ptr %x, align 4
  %cmptmp = icmp slt i32 %x1, 10
  br i1 %cmptmp, label %loop_body, label %loop_end

loop_body:                                        ; preds = %loop_cond
  %x2 = load i32, ptr %x, align 4
  %0 = call i32 (ptr, ...) @printf(ptr @0, i32 %x2)
  %x3 = load i32, ptr %x, align 4
  %addtmp = add i32 %x3, 1
  store i32 %addtmp, ptr %x, align 4
  br label %loop_cond

loop_end:                                         ; preds = %loop_cond
  ret i32 0
}

attributes #0 = { nounwind }

