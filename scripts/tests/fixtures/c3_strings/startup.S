.section .text.start, "ax"
.global _start
_start:
  .option push
  .option norelax
  la gp, __global_pointer$
  .option pop
  la sp, __stack_top
  call main
.global probe_done
probe_done:
  j probe_done
