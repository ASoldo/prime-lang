ENTRY(_start)
EXTERN(prime_value_as_string prime_value_tag)
MEMORY {
  text (rx) : ORIGIN = 0x40380000, LENGTH = 96K
  data (rw) : ORIGIN = 0x3FC90000, LENGTH = 96K
}
SECTIONS {
  .text : { KEEP(*(.text.start)) *(.text .text.*) } >text
  .data : {
    __global_pointer$ = . + 0x800;
    *(.rodata .rodata.* .srodata .srodata.* .sdata .sdata.* .data .data.*)
  } >data
  .bss (NOLOAD) : { *(.sbss .sbss.* .bss .bss.* COMMON) } >data
  __stack_top = ORIGIN(data) + LENGTH(data);
}
