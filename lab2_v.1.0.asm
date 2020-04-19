.model small
.stack 100h
;org 100h
.data
input_str_msg db "Enter some string:",10,13,'$' 
input_word_to_remove_msg db "Enter the word you want to delete:", 10, 13, '$'
error_message db "Error: string length is more then 200", 10, 13, '$'
result_str db "Result:",10,13,'$'

string_length dw 0 
word_to_remove_length dw 0
symbols_to_move dw 0
_offset dw 1 

string_buffer db 200, 201 dup(0dh)
word_to_delete_buffer db 200, 202 dup(0dh)
.code

main proc
    mov ax, @data
    mov ds, ax
    mov es, ax

INPUT_STRING:
    call clear_regs
    lea dx, input_str_msg
    call println

    lea dx, string_buffer
    call getline
    xor dx, dx
    add dl, string_buffer[1]
    cmp dl, 0
    je INPUT_STRING 
    inc dl
    mov string_length, dx 
    inc dx
    mov symbols_to_move, dx
    mov string_buffer[1], ' '

INPUT_WORD_TO_DELETE: 
    call clear_regs
    lea dx, input_word_to_remove_msg
    call println 
    lea dx, word_to_delete_buffer
    call getline 
    xor dx, dx
    add dl, word_to_delete_buffer[1]
    cmp dl, 0
    je END 
    inc dx
    mov word_to_remove_length, dx
    mov word_to_delete_buffer[1], ' '
    add bx, string_length
    sub bx, dx
    jb END
    dec dl
;checking for space or tab
    add al, ' ' 
    add cl, dl
    lea di,  word_to_delete_buffer[2] 
    repne scasb
    je INPUT_WORD_TO_DELETE
    xor al, al
    add al, 09h  ;tab
    xor cl,cl
    add cl,dl
    lea di,  word_to_delete_buffer[2] 
    repne scasb  
    je INPUT_WORD_TO_DELETE
;INTRO ENDS
    call clear_regs
;finding word
    lea si, string_buffer[1]
    lea di, word_to_delete_buffer[1] 
    cld      
    
CYCLE_FINDING:
    mov cx, symbols_to_move
    dec cx
    cmp cx, 0
    jz END 
    mov _offset, 1
    push si
    push di
    push cx
    mov bx, si
    xor cx,cx
    add cx, word_to_remove_length
    repe cmpsb
    je MATCHING_FOUND
    jne NOT_FOUND

MATCHING_FOUND:
    cmp [si], ' '
    je WORD_FOUND
    cmp [si], 0dh
    jne NOT_FOUND

WORD_FOUND:
    call delete_word
    pop cx
    pop di
    pop si
    add si, _offset
    loop CYCLE_FINDING  
    
NOT_FOUND:
    pop cx
    pop di
    pop si
    add si, _offset
    sub symbols_to_move, 1
    loop CYCLE_FINDING 

END:
    lea dx,result_str
    call println 

    call puts_by_char
 
    mov ah, 4ch
    int 21h

endp main  

delete_word proc
    push si                   
    push di 
    push ax 
    push cx
    xor ax,ax  
    xor cx,cx                
    mov cx, symbols_to_move
    sub cx, word_to_remove_length
    mov symbols_to_move, cx    
    mov di, bx                
    repe movsb 
    mov ax, string_length
    sub ax, word_to_remove_length
    mov string_length, ax 
    pop cx
    pop ax                
    pop di                     
    pop si
    ret    
endp delete_word

println proc ; in dx addres of the string
    push ax
    mov ah, 09h
    int 21h
    pop ax
    ret     
endp println   

getline proc ; in dx addres of buffer
    push ax
    push dx    
    mov ah, 0ah
    int 21h
    mov ah, 02h
    mov dl,10
    int 21h
    mov dl, 13
    int 21h
    pop dx    
    pop ax
    ret    
endp getline  

clear_regs proc
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx
    ret    
endp clear_regs

puts_by_char proc
    push si
    push cx
    xor cx,cx  
    lea si, string_buffer[2] 
    add cx, string_length
    dec cx
    mov ah, 02h 
loop_puts_by_char:
    mov dl,[si]
    inc si
    int 21h 
    loopne loop_puts_by_char 
    pop cx
    pop si
    ret
endp puts_by_char