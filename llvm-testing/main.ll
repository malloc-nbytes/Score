HERE: sumHERE: sumHERE: printfdone
; ModuleID = 'global_mod'
source_filename = "global_mod"

@0 = private unnamed_addr constant [8 x i8] c"sum: %d\00", align 1

; Function Attrs: nounwind
declare i32 @printf(ptr nocapture, ...) #0

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
  %s = alloca i32, align 4
  %0 = call i32 (i32, i32, ...) @sum(i32 5, i32 5)
  %1 = call i32 (i32, i32, ...) @sum(i32 10, i32 %0)
  store i32 %1, ptr %s, align 4
  %s1 = load i32, ptr %s, align 4
  %2 = call i32 (ptr, ...) @printf(ptr @0, i32 %s1)
  ret i32 0
}

attributes #0 = { nounwind }

