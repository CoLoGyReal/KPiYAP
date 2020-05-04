.model tiny
.code
org 100h
start:
jmp main
        
numbuf db 9 dup (?)
badnum db "Bad number! Try again: $"  
msg_elem1 db  "mas[$"
msg_elem2 db  "][$"
msg_elem3 db  "] = $"
msg_overflow db  "overflow!$"
mrows   equ 5                       ; col-vo strok
mcols   equ 6                       ; col-vo stolbcov
mcells  equ mrows * mcols           ; razmer 
matrix  dw  mcells dup (?)
msg_min db  "Minimal columns: $"

        
pushAll macro
    push ax
    push bx
    push cx
    push dx
    push si
    push di
endm

popAll macro
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
endm

mcrPrintChar macro char
    mov dl, char
    mov ah, 02h
    int 21h
endm

mcrNewline macro              ; \n
    mcrPrintChar 13
    mcrPrintChar 10
endm

mcrPrintMsg macro msg
    mov ah, 09h
    mov dx, offset msg
    int 21h
endm

printNum proc                          ; vivod chisla      
    arg_num equ word ptr [bp+4]        ; dostaem chislo iz steka
    push bp
    mov bp, sp                         ; v bp zanosim smeschenie na vershiny
    pushAll

    ; neg negative number
    mov ax, arg_num
    cmp ax, 0
    jge printNumNeg
        neg ax
    printNumNeg:

    ; push digits to stack
    mov bx, 0
    printNumConvert:
        mov cx, 10
        mov dx, 0
        div cx
      
        add dl, "0"
        push dx
        inc bx

        cmp ax, 0
        jne printNumConvert

    ; push sign
    cmp arg_num, 0
    jge printNumAfterSign
        push "-"                   ; push - , esli chislo < 0
        inc bx
    printNumAfterSign:

    ; print stack
    printNumPrint:
        pop dx
        dec bx
        mcrPrintChar dl

        cmp bx, 0
        jg printNumPrint

    popAll
    pop bp
    ret 2
printNum endp



inputNum proc                                   ; vvod chisla            
    ; memory on stack for result value
    pop ax
    push ax
    push ax
    result equ word ptr [bp+4]

    push bp
    mov bp, sp
    pushAll

    ; input string
    inputNumRead:
        mov numbuf[0], 7
        mov ah, 0Ah
        mov dx, offset numbuf
        int 21h
        mcrNewline

    ; check string
    mov bx, 0
    mov bl, numbuf[1]
    cmp bx, 0 ;             check if null
    je inputNumValidateAgain
    inputNumValidate:                        ; validation
        dec bx
        mov al, numbuf[2+bx]

        cmp al, '-'
        je inputNumSkipNumCheck
        cmp al, '+'
        je inputNumSkipNumCheck

        cmp al, '0'
        jl inputNumValidateAgain               ; again
        cmp al, '9'
        jg inputNumValidateAgain

        inputNumSkipNumCheck:

        cmp bx, 0
        jg inputNumValidate
    jmp inputNumValidateAfter

    ; if validate failed
    inputNumValidateAgain:
        mov ah, 09h
        mov dx, offset badnum
        int 21h

        jmp inputNumRead
    inputNumValidateAfter:

    ; parse num
    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov si, 0 ; neg flag
    inputNumParse:
        mov cl, numbuf[2+bx]
        cmp cl, '+'
        je inputNumParseNextIter

        cmp cl, '-'
        jne inputNumParseNotNeg              ; esli > 0
            xor si, 1
            jmp inputNumParseNextIter
        inputNumParseNotNeg:

        sub cl, '0'

        ; check overflow and multiple by 10
        cmp ax, 3276 ; max possible value for mul
        jg inputNumValidateAgain
        mov dx, 10
        mul dx

        ; check overflow and add cx
        mov di, 7FFFh ; check max value to add
        sub di, ax                             ; error esli + do perepolnenia
        cmp cx, di
        jg inputNumValidateAgain
        add ax, cx                       ; akym

        inputNumParseNextIter:
        inc bx
        cmp bl, numbuf[1]
        jl inputNumParse

    ; neg if flag active
    cmp si, 1
    jne inputNumNotNeg
        neg ax
    inputNumNotNeg:

    mov result, ax

    popAll
    pop bp
    ret
inputNum endp

printMatrix proc                                    ; vivod matrix
    arg_cols   equ word ptr [bp+4]
    arg_rows   equ word ptr [bp+6]       
    arg_matrix equ word ptr [bp+8]     
    push bp
    mov bp, sp
    pushAll

    mov bx, arg_matrix 
    mov di, 0                           ; stroka
    mov si, 0                           ; stolbec
    printMatrixNext:
        push [bx]
        call printNum
        add bx, 2

        mcrPrintChar 9 ;               vivod 'tab'
        inc si
        cmp si, arg_cols
        jl printMatrixNext

        mcrNewline
        mov si, 0
        inc di
        cmp di, arg_rows
        jl printMatrixNext

    popAll
    pop bp
    ret 6
printMatrix endp



inputMatrix proc                                            ; vvod matrix
    arg_cols   equ word ptr [bp+4]                     ; prisvaivaem
    arg_rows   equ word ptr [bp+6]
    arg_matrix equ word ptr [bp+8]
    push bp
    mov bp, sp
    pushAll

    mov bx, arg_matrix      ; smeschenie matr
    mov di, 0 ; stroki
    mov si, 0 ; stolbci
    inputMatrixNext:    
               
        mcrPrintMsg msg_elem1        ; vivod 'shapki'
        push di
        call printNum
        mcrPrintMsg msg_elem2
        push si
        call printNum
        mcrPrintMsg msg_elem3

        call inputNum                ; vvod chisla
        pop ax
        mov [bx], ax
        add bx, 2

        inc si                       ; perehod na sled element stroki
        cmp si, arg_cols
        jl inputMatrixNext

        mov si, 0                    ; perehod na sled stroky
        inc di
        cmp di, arg_rows
        jl inputMatrixNext

    popAll
    pop bp
    ret 6                           ; vozvrat iz proc s ochistkoi steka na 6 bait  
inputMatrix endp



minColsMatrix proc                                         ; nahogdenie min stolbca
    arg_cols   equ word ptr [bp+4]          ;    
    arg_rows   equ word ptr [bp+6]          ;   dostaem iz steka
    arg_matrix equ word ptr [bp+8]          ;   arg_cols = 6, arg_rows = 5, arg_matrix = 5*6 = 30
    push bp
    mov bp, sp
    pushAll

    mov cx, 7FFFh ;                 min sum  ( 7FFFh = 32767  )
    mov si, 0 ; 1 stroka
    minColsMatrixNextCol:
        mov ax, 0 ;                     summa etogo stolbca
        mov bx, arg_matrix ; ptr
        add bx, si ;                 double bcs word
        add bx, si
        mov di, 0 ;  dly perehoda na 1 element novoi stroki
        minColsMatrixNextRow:
            add ax, [bx]                                ; slogenie elementov stolbca
            jo minColsMatrixFail                        ; proverka flaga perepolnenia 

            add bx, arg_cols ; double bcs word          ; sdvig na next element stolbca
            add bx, arg_cols 
            
            inc di
            cmp di, arg_rows
            jl minColsMatrixNextRow

        cmp ax, cx
        jge minColsMatrixNotLesser
            mov cx, ax                         ; sohranenie novogo znachenia
        minColsMatrixNotLesser:

        inc si
        cmp si, arg_cols
        jl minColsMatrixNextCol

    mov si, 0 ; col 
    
    minColsMatrixOutNextCol:                            ;   bloc dly vivoda nomera stolbca 
        mov ax, 0 ; sum of this col
        mov bx, arg_matrix
        add bx, si
        add bx, si
        mov di, 0 ; row  
        
        minColsMatrixOutNextRow:
            add ax, [bx]                            ; summa stolbca

            add bx, arg_cols
            add bx, arg_cols
            inc di
            cmp di, arg_rows
            jl minColsMatrixOutNextRow

        cmp ax, cx                                 ; sravnenie sohranennoi sum i poluchennoi
        jne minColsMatrixNotEqual
            mov di, si
            inc di
            push di
            call printNum              ; vivod nomera stolbca
            mcrPrintChar " "           ; elsi neskol'ko
            
        minColsMatrixNotEqual:

        inc si
        cmp si, arg_cols
        jl minColsMatrixOutNextCol


    jmp minColsMatrixNoFail                             
    
    minColsMatrixFail:                ; pri perepolnenii
        mcrPrintMsg msg_overflow
    
    minColsMatrixNoFail:

    popAll
    pop bp
    ret 6
minColsMatrix endp


main:
push offset matrix                 ; sohranyem v steke dly ispol'zovania 
push mrows
push mcols
call inputMatrix

push offset matrix
push mrows
push mcols
call printMatrix

mcrPrintMsg msg_min

push offset matrix
push mrows
push mcols
call minColsMatrix


; terminate program
mov ah, 4ch
int 21h

end start
