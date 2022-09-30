[bits 16]
[org 0x7c00]



; set up Fat file system

jmp short start
nop

OEMIdentifier:          db 'MyOS_0.1'
BytesPerSector:         dw 0x200
SectorsPerCluster:      db 0x01
ReservedSectors:        dw 0x01
FatCount:               db 0x02
DirectoryEntryCount:    dw 0xe0
TotalSectors:           dw 0xb40 ; 2880 is number of sectors in a 1.55MB floppy
MediaDescriptorType:    db 0xf0 ; 0xf0 is media descriptor type for a 3.5 inch floppy
SectorsPerFat:          dw 0x09
SectorsPerTrack:        dw 0x12
HeadsPerCylinder:       dw 0x02
HiddenSectors:          dd 0x00 
LargeSectorCount:       dd 0x00

; extended boot record

DriveNumber:            db 0x00
Reserved:               db 0x00               
Signature:              db 0x29               
VolumeID:               db 0x12, 0x34, 0x56, 0x78 ; serial number can by anything
VolumeLabel:            db 'MyOs Disk  '
SystemID:               db 'FAT12   '


start:

; define data and code segment locations

mov ax, 0
mov ds, ax
mov es, ax

; set up stack
mov ss, ax
mov sp, 0x7c00
mov bp, sp


mov si, hello
call disp_string



.read_root_directory:

  ; calculate how many sectors need to be loaded

    mov ax, [DirectoryEntryCount]
    shl ax, 5
    xor dx, dx
    div word [BytesPerSector]
    test dx, dx
    jz .next
    inc ax

  .next:
    push ax ; size of the root directory, used for reading of kernel file

  ; calculate the LBA of the root directory start

    mov ax, [SectorsPerFat]
    mul byte [FatCount]
    add ax, [ReservedSectors]
    push ax ; root directory start, so it can be used when loading the kernel

  ; where the read sectors are to be stored

    mov bx, buffer

  ; drive number to be read from

    mov dl, [DriveNumber]

    call read_disk_sector

    mov si, kernel_name
    mov di, buffer
    xor bx, bx

.search_kernel:

    mov cx, 11
    push di
    repe cmpsb
    pop di
    je .found_kernel

    add di, 0x20
    inc bx
    cmp bx, [DirectoryEntryCount]
    jle .search_kernel


.found_kernel:    
    mov ax, bx
    mul word [DirectoryEntryCount]
    add ax, di
    add ax, 26
    push ax ; first cluster low, used in reading of the kernel

    mov si, pass
    call disp_string

    mov ax, [ReservedSectors] ; location of the FAT table on the floppy disk
    mov bx, buffer ; will overwrite the root directory table
    
    mov cl, [SectorsPerFat]
    call read_disk_sector

    call disp_string

.load_kernel:
    ; calculate the lba of the end of the root directory in bx

    pop ax ; lower cluster value of the kernel

    pop bx ; lba of the root directory
    pop cx ; size of the root directory
    add bx, cx ; lba of root directory end
    mov [buffer], bx ; this word of the FAT table isnt used so can be used for storing a variable
    push ax


    mov ax, [SectorsPerFat]
    mul word [BytesPerSector] ; create an offset for the buffer with size of a FAT table
    mov bx, ax
    add bx, buffer ; add location of buffer to the fat table sized offset

    ;pop ax
    mov ax, 0x02
    mov cl, [SectorsPerCluster]

.lp:

    sub ax, 2 ; refer to lba calculation in readfile function in Fat12.c
    mul byte [SectorsPerCluster]
    add ax, word [buffer] ; ax = lba of cluster to be read
    ;mov [buffer + 1], al
 
    mov dl, [DriveNumber]
 
    call read_disk_sector

    mov si, pass
    call disp_string

    push ax ; add the size of a cluster to the buffer offset
    mov ax, [SectorsPerCluster]
    mul word [BytesPerSector]
    add bx, ax
    
    ;mov al, byte [buffer + 1] ; calculate the FAT index
    pop ax
    push ax
    mov si, 0x03
    mul si
    mov si, 0x02
    div si
    mov di, ax
    
    pop ax

    mov si, 0x02 ; see if the current cluster is even or odd
    div si
    cmp ah, byte 0x00 ; 16 bot DIV with byte divisor stores remainder in AH register
    jnz .odd

.even:
    
    mov ax, buffer
    add ax, di
    and ax, word 0x0fff
    jmp .fini

.odd:
    mov ax, buffer
    add ax, di
    shr ax, 0x04
    

.fini:

    cmp ax, word 0x0ff8
    jl .loaded
    jmp .lp

.loaded:

    mov si, test5
    call disp_string

    mov ax, [SectorsPerFat]
    mul word [BytesPerSector]
    add ax, buffer
    inc ax
    jmp ax


jmp $

disp_string:
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


floppy_failed:
    mov si, Disk_read_Failed
    call disp_string
    jmp keyboard_reboot

keyboard_reboot:
    mov ah, 0
    int 0x16           ; performs a blocking keyboard request
    jmp 0xffff:0x0     ; reset vector


lba_to_chs:
    ; converts lba to chs sector location system
    ; Parameters:
    ;   ax = lba
    ; Returns: 
    ;   cx [bits 0-5]: sector number ch: low eight bits of cylinder number
    ;   cx [bits 6-15]: cylinder number
    ;   ch: low eight bits of cylinder number
    ;   bits 6-7 are high two bits of cylinder number
    ;   dh: head number
    push ax
    push dx

    xor dx, dx ; dx = 0
    div word [SectorsPerTrack] ; ax = lba / SectorsPerTrack
                              ; dx = lba % SectorsPerTrack

    inc dx                    ; dx = (lba % SectorsPerTrack) + 1 = Sectors
    mov cx, dx                ; cx = sectors

    xor dx, dx
    div word [HeadsPerCylinder]; ax = (lba / SectorsPerTrack) / HeadsPerCylinder = cylinder
                               ; dx = (lba / SectorsPerTrack) % HeadsPerCylinder = head

    mov dh, dl                 ; dh = head
    mov ch, al
    shl ah, 6                  ; get the high two bits into bits 6 and 7 of cl
    or cl, ah
                     

    pop ax
    mov dl, al                 ; restore dl as dh is in use
    pop ax
    ret


read_disk_sector:
    ; reads sector(s) from disk
    ; Parameters:
    ;   ax: lba address
    ;   cl: number of sectors to read
    ;   dl: drive number to be read from
    ;   es:bx Where the memory buffer is located (i.e. where the sector is going to be stored)

    push di


    push cx                    ; save cx so it doesn't get overwritten by the fucntion
    call lba_to_chs

    mov ah, 0x02
    mov di, 3                  ; counter as bios reccomends three attempts at loading from a floppy

  .retry:

    pusha
    stc                        ; set the carry flag as this will be cleared if the int 0x13 is successful
    int 0x13
    jnc .finished              ; jump if carry is cleared

    ; failed read
    popa
    call disk_reset
    dec di
    test di, di                ; performs an AND on the two registers and sets the flags as if it were a cmp statement
    jnz .retry

  .Fail:
    jmp floppy_failed

  .finished:
    popa
    pop cx
    pop di
    ret



disk_reset:
    ; resets disk
    ; Param: dl = disk to reset

    pusha

    mov ah, 0
    mov dl, 0
    stc             ; set carry flag which is cleared if int 0x13 succeeds

    int 0x13
    jc floppy_failed    ; if int 0x13 fails go to floppy_failed routine

    popa
    ret

    ; error messages

    Disk_read_Failed: db "Read from disk failed", 0x0d, 0x0a, 0x00   ; 0x0d, 0x0a is the new line command
    hello: db "Loading Kernel", 0x0d, 0x0a, 0x00
    pass: db "Passed", 0x0d, 0x0a, 0x00
    kernel_name db "TEST    BIN"
    test5 db "T5", 0x0d, 0x0a, 0x00


times 510 - ($ - $$) db 0

dw 0xaa55

buffer:
