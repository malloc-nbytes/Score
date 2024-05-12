; ModuleID = 'global module'
source_filename = "global module"

define i32 @sum(i32 %0, i32 %1) {
entry1:                                           ; No predecessors!
  %b = alloca i32, align 4
  %a = alloca i32, align 4

  store i32 %0, ptr %a, align 4
  store i32 %1, ptr %b, align 4
  %a2 = load i32, ptr %a, align 4
  ret i32 %a2
}

define i32 @main() {
entry1:                                           ; No predecessors!
  ret i32 4
}

