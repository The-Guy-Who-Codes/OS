[bits 16]
mov ax, 0x00
mov ds, ax
mov si, Success
call disp_stringg

jmp $

disp_stringg:
    ; displays string
    ; Parameters:
    ;   ds:si start of string

    push si
    push bx
    push cx

    

  .lp:
    lodsb               ; loads character from string into al from location ds:si and then incriments si
    or al, al
    jz .done 
    mov ah, 0x0e        ; has to be here as or overwrites the ax register
    mov bh, 0           ; sets page number to 0
    int 0x10
    jmp .lp

.done:
    pop cx
    pop bx
    pop si
    ret

Success: db "This Worked", 0x0d, 0x0a, 0x00

