; ModuleID = 'global_mod'
source_filename = "global_mod"

; Function Attrs: nounwind
declare i32 @printf(ptr, ...) #0

; Function Attrs: nounwind
declare ptr @malloc(i64) #0

; Function Attrs: nounwind
declare void @free(ptr) #0

attributes #0 = { nounwind }

; ModuleID = 'global_mod'
source_filename = "global_mod"

define i32 @main() {
entry:
  ret i32 0
}

