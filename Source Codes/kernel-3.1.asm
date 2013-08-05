;*****************start of the kernel code***************
[org 0x000]
[bits 16]

[SEGMENT .text]

;START #####################################################
    mov ax, 0x0100			;location where kernel is loaded
    mov ds, ax
    mov es, ax

    cli
    mov ss, ax				;stack segment
    mov sp, 0xFFFF			;stack pointer at 64k limit
    sti

    push dx
    push es
    xor ax, ax
    mov es, ax
    cli
    mov word [es:0x21*4], _int0x21	; setup interrupt service
    mov [es:0x21*4+2], cs
    sti
    pop es
    pop dx

    mov si, strWelcomeMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21
	call _display_endl
	
	mov si, strCredit   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21
	call _display_endl
	call _display_endl
	
	mov si, strCommands   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21
	call _commands

	call _shell				; call the shell

    int 0x19                ; reboot
;END #######################################################

_int0x21:
    _int0x21_ser0x01:       ;service 0x01
    cmp al, 0x01            ;see if service 0x01 wanted
    jne _int0x21_end        ;goto next check (now it is end)

	_int0x21_ser0x01_start:
    lodsb                   ; load next character
    or  al, al              ; test for NUL character
    jz  _int0x21_ser0x01_end
    mov ah, 0x0E            ; BIOS teletype
    mov bh, 0x00            ; display page 0
    mov bl, 0x07            ; text attribute
    int 0x10                ; invoke BIOS
    jmp _int0x21_ser0x01_start
    _int0x21_ser0x01_end:
    jmp _int0x21_end

    _int0x21_end:
    iret

_shell:
	_shell_begin:
	;move to next line
	call _display_endl

	;display prompt
	call _display_prompt

	;get user command
	call _get_command

	;split command into components
	call _split_cmd

	;check command & perform action

	; empty command
	_cmd_none:
	mov si, strCmd0
	cmp BYTE [si], 0x00
	jne	_cmd_ver		;next command
	jmp _cmd_done

	; display version
	_cmd_ver:
	mov si, strCmd0
	mov di, cmdVer
	mov cx, 4
	repe	cmpsb
	jne	_cmd_help		;next command

	call _display_endl
	call _display_endl
	mov si, strOsName		;display version
	mov al, 0x01
    int 0x21
	call _display_space
	mov si, txtVersion		;display version
	mov al, 0x01
    int 0x21
	call _display_space

	mov si, strMajorVer
	mov al, 0x01
    int 0x21
	mov si, strMinorVer
	mov al, 0x01
    int 0x21
	call _display_endl
	jmp _cmd_done

	_cmd_help:
	mov si, strCmd0
	mov di, cmdHelp
	mov cx, 4
	repe cmpsb
	jne _cmd_HardInfo  ;next command
	
	call _commands
	jmp _cmd_done

	_cmd_HardInfo:
	mov si, strCmd0
	mov di, cmdHardware
	mov cx, 6
	repe	cmpsb
	jne	_cmd_exit		;next command

		
	call _display_endl
	call _display_endl
	mov si, strHDDetail
	mov al, 0x01
    int 0x21

	call _display_endl
	call _cpu
	call _memory
	call _floppy
	call _harddisk
	call _serial_ports	
	call _parallel_ports
	call _keyboard
	call _display_endl
	
	jmp _cmd_done		


	; exit shell
	_cmd_exit:
	mov si, strCmd0
	mov di, cmdExit
	mov cx, 4
	repe	cmpsb
	jne	_cmd_unknown		;next command

	je _shell_end			;exit from shell

	_cmd_unknown:
	call _display_endl
	mov si, msgUnknownCmd		;unknown command
	mov al, 0x01
    int 0x21

	_cmd_done:

	;call _display_endl
	jmp _shell_begin

	_shell_end:
	ret

_get_command:
	;initiate count
	mov BYTE [cmdChrCnt], 0x00
	mov di, strUserCmd

	_get_cmd_start:
	mov ah, 0x10		;get character
	int 0x16

	cmp al, 0x00		;check if extended key
	je _extended_key
	cmp al, 0xE0		;check if new extended key
	je _extended_key

	cmp al, 0x08		;check if backspace pressed
	je _backspace_key

	cmp al, 0x0D		;check if Enter pressed
	je _enter_key

	mov bh, [cmdMaxLen]		;check if maxlen reached
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start

	;add char to buffer, display it and start again
	mov [di], al			;add char to buffer
	inc di					;increment buffer pointer
	inc BYTE [cmdChrCnt]	;inc count

	mov ah, 0x0E			;display character
	mov bl, 0x07
	int 0x10
	jmp	_get_cmd_start

	_extended_key:			;extended key - do nothing now
	jmp _get_cmd_start

	_backspace_key:
	mov bh, 0x00			;check if count = 0
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start		;yes, do nothing

	dec BYTE [cmdChrCnt]	;dec count
	dec di

	;check if beginning of line
	mov	ah, 0x03		;read cursor position
	mov bh, 0x00
	int 0x10

	cmp dl, 0x00
	jne	_move_back
	dec dh
	mov dl, 79
	mov ah, 0x02
	int 0x10

	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_move_back:
	mov ah, 0x0E		; BIOS teletype acts on backspace!
    mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_enter_key:
	mov BYTE [di], 0x00
	ret

_split_cmd:
	;adjust si/di
	mov si, strUserCmd
	;mov di, strCmd0

	;move blanks
	_split_mb0_start:
	cmp BYTE [si], 0x20
	je _split_mb0_nb
	jmp _split_mb0_end

	_split_mb0_nb:
	inc si
	jmp _split_mb0_start

	_split_mb0_end:
	mov di, strCmd0

	_split_1_start:			;get first string
	cmp BYTE [si], 0x20
	je _split_1_end
	cmp BYTE [si], 0x00
	je _split_1_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_1_start

	_split_1_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb1_start:
	cmp BYTE [si], 0x20
	je _split_mb1_nb
	jmp _split_mb1_end

	_split_mb1_nb:
	inc si
	jmp _split_mb1_start

	_split_mb1_end:
	mov di, strCmd1

	_split_2_start:			;get second string
	cmp BYTE [si], 0x20
	je _split_2_end
	cmp BYTE [si], 0x00
	je _split_2_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_2_start

	_split_2_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb2_start:
	cmp BYTE [si], 0x20
	je _split_mb2_nb
	jmp _split_mb2_end

	_split_mb2_nb:
	inc si
	jmp _split_mb2_start

	_split_mb2_end:
	mov di, strCmd2

	_split_3_start:			;get third string
	cmp BYTE [si], 0x20
	je _split_3_end
	cmp BYTE [si], 0x00
	je _split_3_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_3_start

	_split_3_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb3_start:
	cmp BYTE [si], 0x20
	je _split_mb3_nb
	jmp _split_mb3_end

	_split_mb3_nb:
	inc si
	jmp _split_mb3_start

	_split_mb3_end:
	mov di, strCmd3

	_split_4_start:			;get fourth string
	cmp BYTE [si], 0x20
	je _split_4_end
	cmp BYTE [si], 0x00
	je _split_4_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_4_start

	_split_4_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb4_start:
	cmp BYTE [si], 0x20
	je _split_mb4_nb
	jmp _split_mb4_end

	_split_mb4_nb:
	inc si
	jmp _split_mb4_start

	_split_mb4_end:
	mov di, strCmd4

	_split_5_start:			;get last string
	cmp BYTE [si], 0x20
	je _split_5_end
	cmp BYTE [si], 0x00
	je _split_5_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_5_start

	_split_5_end:
	mov BYTE [di], 0x00

	ret

_display_space:
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
	ret

_display_endl:
	mov ah, 0x0E		; BIOS teletype acts on newline!
    mov al, 0x0D
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x0E		; BIOS teletype acts on linefeed!
    mov al, 0x0A
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	ret
	
_display_prompt:
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	ret

_commands:
	call _display_endl
	call _display_endl
	mov si, strList
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strVer
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strHardware
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strReboot
	mov al, 0x01
	int 0x21
	call _display_endl	
	ret
			
_cpu:
	call _display_endl
	mov si, strCPU
	mov al, 0x01
	int 0x21
	mov si, strCPUInfo
	
	xor   EAX,EAX  
	mov eax, 0x80000002
	cpuid
	call _capture
	mov eax, 0x80000003
	cpuid
	call _capture
	mov eax, 0x80000004
	cpuid
	call _capture
	add si,16
	mov si,0x00

	mov si, strCPUInfo
	mov al, 0x01
	int 0x21
      ret
	  
_memory:
	call _display_endl
	
	xor ax,ax
	xor bx,bx
	xor cx,cx
	xor dx,dx
	
	mov ax, 0xe801
	int 0x15
	jc _undetect
	cmp ah, 0x86		
	je _undetect
	cmp ah, 0x80		
	je _undetect
	
	mov si, strMemory
	mov al, 0x01
	int 0x21
	cmp cx, 0x0000
	je _copy
	jmp _get_total

	_copy:
	mov cx,ax		;copy AX to CX
	mov dx,bx		;copy BX to DX

	_get_total:
	shr dx, 4		;divide dx by 2^4
	shr cx, 10		;divide cx by 2^10 (to convert to MBs)
	add cx,dx		;get the total memory
	mov dx, cx
	call _hex2dec
	mov si, strMB
	mov al, 0x01
	int 0x21
	jmp _end_cmd

	_undetect:
	mov si, strUndetect
	mov al, 0x01
	int 0x21	
	
	_end_cmd:
	ret
	
_harddisk:
	call _display_endl

	mov si, strHardDisk
	mov al, 0x01
	int 0x21

	mov ax, 0x0040
	push es
	mov es,ax
	mov al,[es:0x0075]	; read 40:75 
	add al, 48			
	pop es
	
	mov ah, 0x0e
	int 0x10
	ret
	
_floppy:
	call _display_endl
	mov si, strFloppy
	mov al, 0x01
	int 0x21
	
	int 0x11
	and ax, 1
	cmp ax, 1
	je _floppy_present
	mov ah, 0x0e
	mov al, '0'
	int 0x10
	ret

	_floppy_present:
	xor ax, ax
	int 0x11
	and ax, 0xc0
	shr ax, 6
	add ax, 48
	
	mov ah, 0x0e
	int 0x10
	ret

_serial_ports:
	call _display_endl
	
	mov si, strSerial
	mov al, 0x01
	int 0x21
		
	xor ax, ax
	int 0x11
	and ax, 0xe00
	shr ax, 9
	add ax, 48
	
	mov ah, 0x0e
	int 0x10
	ret 
	
_parallel_ports:
	call _display_endl

	mov si, strParallel
	mov al, 0x01
	int 0x21

	xor ax, ax
	int 0x11
	and ax, 0xc000	;mask out bits 15-14 : number of parallel ports
	shr ax, 14
	add ax, 48		; add 48 to get ASCII char

	mov ah, 0x0e
	int 0x10
	ret
	
_keyboard:
	call _display_endl

	mov si, strKeyboard
	mov al, 0x01
	int 0x21

	xor ax, ax
	xor bx, bx
	mov ax, 0x0040
	push es
	mov es,ax
	mov bx,[es:0x0096]	; read 40:96
	and bx,0x10
	jnz _keyboard_Present
	mov si, strNKPresent
	mov al, 0x01
	int 0x21
	pop es
	ret
	
	_keyboard_Present:
	mov si, strKPresent
	mov al, 0x01
	int 0x21
	pop es
	ret
	
_disp_str:
	lodsb 				; load next character
	or al, al 			; test for NUL character
	jz _printed
	mov ah, 0x0E 		; BIOS teletype
	mov bh, 0x00 		; display page 0
	mov bl, 0x07 		; text attribute
	int 0x10 			
	jmp _disp_str

	_printed:
	ret
	
_capture:
	mov dword [si], eax
	mov dword [si+4], ebx
	mov dword [si+8], ecx
	mov dword [si+12], edx
	add si, 16
	ret

_hex2dec:
	;converts the number in dx to decimal and print it
	push ax
	push bx
	push cx
	push si
	mov ax,dx                ; copy number into AX
	mov si,10                ; SI will be our divisor
	xor cx,cx                ; clean up the CX

	_non_zero:
	xor dx,dx                ; clear DX
	div si                   ; divide by 10
	push dx                  ; push number onto the stack
	inc cx                   ; increment CX to do it more times
	or ax,ax                 ; end of the number?
	jne _non_zero             ; if not repeat

	_write_digits:
	pop dx                   ; get the digit off DX
	add dl,48                ; add 48 to get ASCII
	mov al, dl
	mov ah, 0x0e
	int 0x10
	loop _write_digits

	pop si
	pop cx
	pop bx
	pop ax
	ret

	_print_char:
	push ax                  ; save that AX register
	mov al, dl
	mov ah, 0x0E            ; BIOS teletype acts on newline!
	mov bh, 0x00
	mov bl, 0x07
	int 0x10

	pop ax                   ; restore that AX register
	ret	
	
[SEGMENT .data]
    strWelcomeMsg   db  "Welcome to JOSH Ver 3.1", 0x00
	strCredit   	db  "Modified by Isuru Jayaweera", 0x00
	strPrompt		db	"JOSH>>", 0x00
	strCommands		db	"List of available commands",0x00
	strList			db	"help   <view available commands>",0x00
	strVer			db	"ver    <view version>",0x00
	strHardware		db	"diaghd <view hardware information>",0x00
	strReboot		db	"exit   <reboot>",0x00
	cmdMaxLen		db	255			;maximum length of commands

	strOsName		db	"JOSH", 0x00	;OS details
	strMajorVer		db	"3", 0x00
	strMinorVer		db	".1", 0x00
	
	strKPresent		db	"Present",0x00
	strNKPresent	db	"Not Present",0x00
	strMB			db	" MB",0x00
	strUndetect 	db	"Error in memory detection",0x00
	
	strHDDetail		db	"        Hardware Details of this Computer",0x00
	strCPU			db	"- CPU details              : ",0x00
	strMemory		db	"- Main Memory Details      : ",0x00
	strHardDisk		db	"- Number of Hard drives    : ",0x00
	strFloppy		db	"- Number of Floppy drives  : ",0x00
	strSerial 		db	"- Number of Serial ports   : ",0x00
	strParallel 	db	"- Number of Parallel ports : ",0x00
	strKeyboard		db	"- 101/102 enhance keyboard : ",0x00
	
	cmdHelp			db	"help", 0x00		; internal commands
	cmdVer			db	"ver", 0x00		
	cmdHardware		db	"diaghd", 0x00
	cmdExit			db	"exit", 0x00
	

	txtVersion		db	"version", 0x00	;messages and other strings
	msgUnknownCmd	db	"Unknown command or bad file name!", 0x00

[SEGMENT .bss]
	strUserCmd	resb	256		;buffer for user commands
	cmdChrCnt	resb	1		;count of characters
	strCmd0		resb	256		;buffers for the command components
	strCmd1		resb	256
	strCmd2		resb	256
	strCmd3		resb	256
	strCmd4		resb	256
	strCPUInfo	resb	256		;string variable

;********************end of the kernel code********************
