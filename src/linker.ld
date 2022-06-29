ENTRY(_start)

PHDRS {
    none PT_NULL FLAGS(0);
}

SECTIONS {
    . = 0x40100000;

    .blob : {
        __blob_begin = .;
        *(.text.entry)
        *(.text*)
        *(.data*)
        *(.rodata*)
        *(.bss*)
        . = ALIGN(16);
        __blob_end = .;
    }
}
