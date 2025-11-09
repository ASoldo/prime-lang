; ModuleID = 'prime'
source_filename = "prime"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define void @main() {
entry:
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt, i32 15)
  ret void
}

declare i32 @printf(ptr, ...)
