printstr:
    mov bp, sp
    pusha
    
    
    mov si, [bp + 2]
    mov ah, 0x0e
    mov al, [si]
    

    .lp:
        cmp al, byte 0x00
        je .exit
        mov al, [si]
        int 0x10
        inc si
        jmp .lp

    .exit:
        popa
        ret



cls:
    push ax
    mov ah, 0x00
    mov al, 0x03
    int 0x10
    pop ax
    ret