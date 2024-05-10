; ModuleID = 'changeme'
source_filename = "changeme"

define i32 @sum(i32 %0, i32 %1) {
entry1:                                           ; No predecessors!
  %addtmp = add i32 100, %1
  %s = alloca i32, align 4
  store i32 %addtmp, ptr %s, align 4
  ret ptr %s
}

define i32 @main() {
entry1:                                           ; No predecessors!
  ret i32 3
}
