ENTRY(platformMain)

PHDRS {
    null    PT_NULL FLAGS(0) ;              /* Null segment */
    text    PT_LOAD FLAGS(1 << 0 | 1 << 2); /* Read + execute */
    rodata  PT_LOAD FLAGS(1 << 2);          /* Read only */
    data    PT_LOAD FLAGS(1 << 1 | 1 << 2); /* Read + write */
}

SECTIONS {
    . = 0xFFFFFFFF80000000;

    text_begin = .;

    .text : {
        *(.text .text.*)
    } : text

    text_end = .;

    . += CONSTANT(MAXPAGESIZE) - 1;
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    rodata_begin = .;

    .rodata : {
        *(.rodata .rodata.*)
    } : rodata
    
    rodata_end = .;

    . += CONSTANT(MAXPAGESIZE) - 1;
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    data_begin = .;

    .data : {
        *(.data .data.*)
    } : data

    .bss : {
        *(.bss .bss.*)
    } : data

    data_end = .;
}
