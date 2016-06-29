; ###############################################
; #		Kratos Version : 1.0		#
; ###############################################


bits	16

org 0x500

jmp	main

	CR		equ 13d
	LF		equ 10d
	TAB		equ 09d
	TAB_CHR		equ ' '

	prompt		db CR,LF,"Input : ",0
	input		times 128 db 0	; Input Buffer Of 128 Bytes
	str_ver		db CR,LF,CR,LF,CR,"        ",\
			   "Version : 1.0",CR,LF,0
	str_help	db CR,LF,CR,LF,CR,"        ",\
			   "Type VER For Version !!!",\
			   CR,LF,CR,"        ",\
			   "Type CLS To Clear Screen !!!",\
			   CR,LF,CR,"        ",\
			   "Type HELP To Display This Message !!!",\
			   CR,LF,0
	str_welcome	db "Welcome To My Operating System !!!",0
	str_default	db CR,LF,CR,LF,CR,"        ",\
			   "Invalid Command !!!",\
			   CR,LF,CR,"        ",\
			   "Type HELP To Display Help !!!",\
			   CR,LF,0
	cmd_ver		db "VER",0
	cmd_help	db "HELP",0
	cmd_cls		db "CLS",0

; +++++++++++++++++++++++++++++++++++++++
; +	Entry Point			+
; +++++++++++++++++++++++++++++++++++++++

	main:
	cli			; clear interrupts
	xor	ax, ax		; null segments
	mov	ds, ax
	mov	es, ax
	mov	ax, 0x0000	; stack begins at 0x0000-0xffff
	mov	ss, ax
	mov	sp, 0xFFFF
	sti			; enable interrupts

	xor	ax,ax		; Select Video Mode Function
	mov	al, 03h		; Select 80x25 (8 Pages) Text Mode
	int	10h		; Call	Interrupt
	mov	dh, 0h		; Row
	mov	dl, 0h		; Column
	call	goto_xy
	mov	cx, 30h
	lea	si, [str_welcome]
	call	put_str

	Begin0:
		lea 	si, [prompt]		; Display Prompt
		mov	cx, 0ah			; Max Length=10
		call 	put_str
		call	beep
		lea 	si, [input]
		mov	cx, 40h			; Max Length=64
		call 	get_str			; Read User Input
		call	del_whitespace		; Do Away Leading And Trailing Whitespace
		call	str_to_upper		; Convert To Uppercase
		call	chk_internal		; Cross-Check In Internal Commands
		call	null_str		; Recycle The Input Buffer
		jmp	Begin0			; Loop Forever

; +++++++++++++++++++++++++++++++++++++++
; +	Delete Whitespace Characters	+
; +++++++++++++++++++++++++++++++++++++++

	del_whitespace:
		push	si		; Save SI
		push	di		; Save DI
		push	dx		; Save DX
		push	cx		; Save CX
		push	bx		; Save BX
		push	ax		; Save AX
		xor	ax, ax
		xor	bx, bx
		xor	cx, cx
		xor	dx, dx
		mov	di, si
		dec	si		; SI=SI-1
	.loop00:
		inc	si		; Go On Incrementing Source String Index
		xor	dx, dx
		mov	dx, [si]
		xor	dh, dh
		cmp	dl, 00h		; Is String Finished ?
		jz	.chomp00
		cmp	dl, 20h		; Is It A Space
		jz	.loop00		; Go On Eating Spaces
		cmp	dl, TAB		; Is It A TAB
		jz	.loop00		; Go On Eating TABS
		push	si		; First Non-Whitespace Character Index In String
		inc	cx		; Number Of Tokens In String
	.loop01:
		inc	si		; Increment SI
		mov	dx, [si]
		xor	dh, dh
		cmp	dx, 00h		; Is String Finished ?
		jz	.chomp00	; Cut Out Useful Part
		cmp	dx, 20h		; Check For Space
		jz	.loop00
		cmp	dx, TAB		; Check For TAB
		jnz	.loop01		; Read On Until Next TAB
		jz	.loop00
	.chomp00:
		cmp	cx, 00h		; Null Input
		jz	.over01		; Return Then
		dec	cx		; Otherwise Decrement Number Of Tokens
		pop	si		; Start Of Finishing Token
		mov	ax, si		; Save It, Just In Case It Becomes Also The Start Of The First Token
	.bypass:
		inc	si		; Increment String Index
		mov	dx, [si]
		xor	dh, dh
		cmp	dx, 00h		; Has String Ended?
		jz	.loop002
		cmp	dx, 20h
		jz	.loop002
		cmp	dx, TAB
		jnz	.bypass		; Bypass All Characters In Token Until First Whitespace
	.loop002:
		mov	bx, si		; Found End
		cmp	cx, 00h		; Is There Only One Token?
		jz	.inst00		; Then Start Of Finishing Token=Start Of Opening Token
	.loop02:
		dec	cx
		pop	ax		; Move Start Of Previous Token In AX
		cmp	cx, 00h		; All Tokens Finished?
		jz	.inst00
		jnz	.loop02		; Loop Over
	.inst00:
		mov	si, ax		; Set SI To Start
	.loop03:
		cmp	si, bx		; All Characters In Token Processed ?
		jz	.over00		; Done, Return
		mov	dx, [si]
		mov	[di], dx	; Otherwise Overwrite Input String
		inc	si		; Increment User Token Input Index
		inc	di		; Increment Processed Token Input Index
		jmp	.loop03		; Loop Over

	.over00:
		xor	dx, dx
		mov	[di], dx	; NULL-Terminate Processed Token
	.over01:
		pop	ax		; Retrieve AX
		pop	bx		; Retrieve BX
		pop	cx		; Retrieve CX
		pop	dx		; Retrieve DX
		pop	di		; Retrieve DI
		pop	si		; Retrieve SI
		ret			; Return
	

; +++++++++++++++++++++++++++++++++++++++
; +	Null Entire String		+
; +++++++++++++++++++++++++++++++++++++++

	null_str:
		push	si		; Save SI
		push	cx		; Save CX
		push	ax		; Save AX
		xor	ax, ax
		call	str_len		; Move Length Of Typed Command In CX
	.more0:
		cmp	cx, 0		; Is It Zero-Length ?
		jz	.finish0	; If So Do Away With
		mov	[si], ax	; Null A Character In Input Buffer
		dec	cx		; Decrement Counter
		inc	si		; Advance SI
		jmp	.more0		; Loop On Until First 0
	.finish0:
		pop	ax		; Retrieve AX
		pop	cx		; Retrieve BX
		pop	si		; Retrieve SI
		ret			; Return
	

; +++++++++++++++++++++++++++++++++++++++
; +	Get Cursor Position		+
; +++++++++++++++++++++++++++++++++++++++

	get_xy :
		push	ax		; Save AX
		xor	ax, ax
		mov	ah, 03h		; Select Put Cursor Function
		int	10h		; Call Interrupt
		pop	ax		; Retrieve AX
		ret

; +++++++++++++++++++++++++++++++++++++++
; +	Place Cursor At Position	+
; +++++++++++++++++++++++++++++++++++++++

	goto_xy :
		push	ax		; Save AX
		push	bx		; Save BX
		push	cx		; Save CX
		push	dx		; Save DX
		xor	ax, ax
		xor	cx, cx
		mov	ah, 02h		; Select Put Cursor Function
		mov	bh, 00h		; Select Page (0-7)
		int	10h		; Call Interrupt
		pop	dx		; Retrieve DX
		pop	cx		; Retrieve CX
		pop	bx		; Retrieve BX
		pop	ax		; Retrieve AX
		ret
		

; +++++++++++++++++++++++++++++++++++++++
; +	Clears The Screen		+
; +++++++++++++++++++++++++++++++++++++++

	clr_scr:
	push	ax		; Save AX
	push	bx		; Save BX
	push	cx		; Save CX
	push	dx		; Save DX
	xor	dx, dx		; Cursor At Top-Left
	call	goto_xy
	mov	ah, 6		; Scroll Up Whole Screen
	mov	al, 0		; Text Color : White
	mov	bh, 7		; Background : Black
	xor	cx, cx		; Top-left
	mov	dh, 24		; Bottom-Most
	mov	dl, 79		; Right-Most
	int	10h
	pop	dx		; Retrieve DX
	pop	cx		; Retrieve CX
	pop	bx		; Retrieve BX
	pop	ax		; Retrieve AX
	ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Check Internal Commands		+
; +++++++++++++++++++++++++++++++++++++++

	chk_internal:
		push	si			; Save SI		
		push	cx			; Save CX
	cmd_ver_lb:
		lea	di, [cmd_ver]		; VER Command
		call	cmp_str			; Compare User Input
		jnc	do_ver			; Execute Command If Matched
	cmd_help_lb:
		lea	di, [cmd_help]		; HELP Command
		call	cmp_str			; Compare User Input
		jnc	do_help			; Execute Command If Matched
	cmd_cls_lb:
		lea	di, [cmd_cls]		; CLS Command
		call	cmp_str			; Compare User Input
		jnc	do_cls			; Execute Command If Matched
	default_lb:
		lea	si, [str_default]	; Default Error Message
		mov	cx, 60h			; Max Length 60Hex Characters
		call	put_str
		jmp	clean0			; Return
	do_ver:
		lea	si, [str_ver]		; String For VER
		mov	cx, 40h			; Max Length 40Hex Characters
		call	put_str
		jmp	clean0			; Return
	do_help:
		lea	si, [str_help]		; String For HELP
		mov	cx, 80h			; Max Length 80Hex Characters
		call	put_str
		jmp	clean0			; Return
	do_cls:
		call	clr_scr			; Call Clear Screen Function
		jmp	clean0			; Return
	clean0:
		pop	cx			; Retrieve CX
		pop	si			; Retrieve SI
		ret				; Return
	

; +++++++++++++++++++++++++++++++++++++++
; +	Compare Strings			+
; +++++++++++++++++++++++++++++++++++++++

	cmp_str:

		push	si				; Save SI
		push	di				; Save DI
		push	ax				; Save AX
		push	bx				; Save BX
		push	cx				; Save CX
		clc					; Default : Clear Carry
		call	str_len				; String Length Of SI
		mov	ax, cx				; Copy String-Length Of SI In AX
		push	si				; Save SI
		mov	bx, di
		mov	si, bx				; Move DI to SI
		call	str_len				; String Length Of DI
		mov	bx, cx				; Copy String-Length Of DI In BX
		pop	si				; Retrieve Back SI
		cmp	ax, bx				; Check If String Lengths Are Equal
		jnz	nosame				; Not Same
		Loop0:
			mov	al, [si]	; Load Next Character From SI to AL
			mov	bl, [di]	; Load Next Character From DI to BL
			cmp	al, bl		; Compare Two Characters
			jnz	nosame		; Not Same
			or	al, al		; Check If AL=0
			jz	Loop0Done	; AL=0? Then Return
			inc	si		; Increment SI
			inc	di		; Increment DI
			jmp	Loop0
		nosame :
			stc			; Set Carry Flag
		Loop0Done:
			pop	cx		; Retrieve CX
			pop	bx		; Retrieve BX
			pop	ax		; Retrieve AX
			pop	di		; Retrieve DI
			pop	si		; Retrieve SI
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Print String			+
; +++++++++++++++++++++++++++++++++++++++

	put_str:
		
		push	si			; Save SI
		push	ax			; Save AX
		push	cx			; Save CX
		Print:
			cmp	cx, 0h		; Check If CX=0
			jz	PrintDone	; Don't Bother Printing Further
			lodsb			; Load Next Character From SI to AL
			or	al, al		; Check If AL=0
			jz	PrintDone	; AL=0? Then Return
			call	put_chr		; Else Print Character
			dec	cx		; Decrement Counter
			jmp	Print
		PrintDone:
			pop	cx		; Retrieve CX
			pop	ax		; Retrieve AX
			pop	si		; Retrieve SI
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Print Character			+
; +++++++++++++++++++++++++++++++++++++++

	put_chr:
			
			push	ax		; Save AX
			push	dx
			cmp	al, TAB		; Check For Tab Character
			jnz	no_tab		; Skip Tab Processing
			push	cx

			push	bx
			xor	bx, bx		; Set Video Page 0
			call	get_xy		; Get XY In DX
			pop	bx
			and	dl, 07h		; Mask Out Remaining Bits But Remainder Of 8 (al%8)
			mov	cx, 08h		; Maximum No. Of Spaces In TAB
			sub	cl, dl		; No. Of Spaces To Print For TAB

		.tc0:
			mov	al, TAB_CHR
			call	put_chr
			loop	.tc0

			pop	cx
			jmp	key_tab		; Don't Print ASCII 09d (TAB)
		no_tab:
			mov	ah, 0eh		; Select Print Character Function

	push	bx
	push	cx
	push	dx
	mov	cx, 01h
	xor	bx, bx
	mov	bl, 00101010b

			int	10h		; Print Character
call	get_xy
dec	dl
call	goto_xy

	call	get_xy
	cmp	dl, 79d
	jz	.next_line
	jmp	.same_line
	.next_line:
	inc	dh
	xor	dl, dl
	jmp	.further0
	.same_line:
	inc	dl
	.further0:
	call	goto_xy
	pop	dx
	pop	cx
	pop	bx

		key_tab:
			pop	dx
			pop	ax		; Retrieve AX
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Get Character Without Printing	+
; +++++++++++++++++++++++++++++++++++++++

	get_ch:

		ReadLoop1:
			mov     ah, 00h		; Read Key Opcode
                	int     16h
                	cmp     al, 0		; ALT, SHIFT, CTRL etc
                	jz      ReadLoop1       ; If so, don't echo this keystroke
			ret
		

; +++++++++++++++++++++++++++++++++++++++
; +	Get And Print Character		+
; +++++++++++++++++++++++++++++++++++++++

	get_chr:

		ReadLoop2:
			mov     ah, 0		; Read Key Opcode
                	int     16h
                	cmp     al, 0		; ALT, SHIFT, CTRL etc
                	jz      ReadLoop2       ; If So, Don't Echo This Keystroke
                	call	put_chr		; Echo Character
			ret
		

; +++++++++++++++++++++++++++++++++++++++
; +	Read String			+
; +++++++++++++++++++++++++++++++++++++++

	get_str:

			push	si			; Save SI
			push	ax			; Save AX
			push	bx			; Save BX
			push	cx			; Save CX
			mov	bx, si			; Copy Initial Address
		Count0:
			call	get_ch
			cmp	al, 08d
			jz	.bksp
			cmp	al, 13d
			jz	Count0Done
			cmp	al, 09d			; Check TAB Key
			jz	.next
			cmp	al, 32d			; Ignore Control Characters	
			jl	Count0
			cmp	cx, 0h
			jz	Count0
			jmp	.next
		.bksp:
			cmp	bx, si			; Is It The First Key ?
			jz	Count0			; Go Back Then

			push	ax			; Save AX
			dec	si			; Reduce One Character
			inc	cx			; Free Up Deleted Character
			cmp	[si], BYTE TAB		; Is It A TAB ?
			jz	.is_tab

			push	bx
			push	cx
			push	dx
			xor	bx, bx			; Select Video Page 0
			call	get_xy			; Read Cursor Position In DX
			cmp	dl, 0h			; Is It The First Column ?
			jz	.f_col00
			mov	al, 08h			; Print Backspace
			call	put_chr
			mov	al, 20h			; Print Space
			call	put_chr
			mov	al, 08h			; Print Backspace
			call	put_chr
			jmp	.n_col00
		.f_col00:
			dec	dh			; Go To Previous Line
							; Assumes DH>0
			mov	dl, 79d			; Max Numbers Of Columns
			call	goto_xy			; Place Cursor
			mov	al, 20h			; Print Space
			call	put_chr
			mov	dl, 79d			; Max Numbers Of Columns
			call	goto_xy			; Place Cursor
		.n_col00:
			pop	dx
			pop	cx
			pop	bx
			jmp	.not_tab

		.is_tab:
			push	cx

			push	ax
			push	bx
			push	dx
			push	si
			push	di
			push	sp
			push	bp
			xor	bx, bx
			call	get_xy
			push	dx			; Save Cursor Before TAB Delete
			xor	cx, cx
		.gt0:
			mov	al, 08h
			call	put_chr			; Go To Previous Character Printed
			xor	bx, bx
			mov	ah, 08h			; Read Character At Cursor
			int	10h			; Call Interrupt
			inc	cx
			cmp	cx, 8h			; Have Max No. Of Spaces For
			jg	.gt1			; TAB Characters Been Processed?
			cmp	al, TAB_CHR		; Go On Processing TAB (Spaces)
			jz	.gt0			; Otherwise
		.gt1:
			dec	cx			; Decrease Once
			pop	dx			; Decrease Once
			xor	bx, bx
			call	goto_xy			; Restore Cursor Saved At Start
			pop	bp
			pop	sp
			pop	di
			pop	si
			pop	dx
			pop	bx
			pop	ax

			push	bx
			push	cx
			push	si			; Save Buffer Index
			xor	bx, bx
			mov	cx, 07h			; Max No. Of Characters In Buffer Before TAB
							; That Could Be Potentially Spaces (TAB Characters)
		.lp0:
			inc	bx			; Increase With Each Space Found
			dec	si			; Go On Checking Buffer For Spaces
			cmp	[si], BYTE TAB_CHR
			jnz	.lp1
			loop	.lp0
		.lp1:
			dec	bx
			pop	si
			pop	cx
			sub	cx, bx			; Reduce From TAB No. Of Spaces (TAB Characters) In Buffer
			pop	bx

		.loop_tab:
			push	bx
			push	cx
			push	dx
			xor	bx, bx			; Select Video Page 0
			call	get_xy			; Read Cursor Position In DX
			cmp	dl, 0h			; Is It The First Column ?
			jz	.f_col01
			mov	al, 08h			; Print Backspace
			call	put_chr
			mov	al, 20h			; Print Space
			call	put_chr
			mov	al, 08h			; Print Backspace
			call	put_chr
			jmp	.n_col01
		.f_col01:
			dec	dh			; Go To Previous Line
							; Assumes DH>0
			mov	dl, 79d			; Max Numbers Of Columns
			call	goto_xy			; Place Cursor
			mov	al, 20h			; Print Space
			call	put_chr
			mov	dl, 79d			; Max Numbers Of Columns
			call	goto_xy			; Place Cursor
		.n_col01:
			pop	dx
			pop	cx
			pop	bx

			dec	cx
			cmp	cx, 0
			jnz	.loop_tab
			pop	cx
		.not_tab:
			xor	ax, ax
			mov	[si], ax		; Reset Deleted Character To 0

			pop	ax
			jmp	Count0
		.next:
			cmp	cx, 0h			; Is Buffer Already Full ?
			jz	Count0			; Go Back And Wait For BKSP or ENTER
			dec	cx			; Decrement Max String Length
			call	put_chr
			mov	[si], al
			inc	si			; Increment SI
			jmp	Count0
		Count0Done:
			pop	cx			; Retrieve CX
			pop	bx			; Retrieve BX
			pop	ax			; Retrieve AX
			pop	si			; Retrieve SI
			ret
		

; +++++++++++++++++++++++++++++++++++++++
; +	Count String Length		+
; +++++++++++++++++++++++++++++++++++++++

	str_len:
			push	ax		; Save AX
			push	si		; Save SI
			xor	cx, cx		; Initialize Counter
		Count:
			lodsb			; Load Next Character From SI to AL
			or	al, al		; Check If AL=0
			jz	CountDone	; AL=0? Then Return
			inc	cx
			jmp	Count
		CountDone:
			pop	si		; Retrieve SI
			pop	ax		; Retrieve AX
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Character To Uppercase		+
; +++++++++++++++++++++++++++++++++++++++

	chr_to_upper:
		
			push	bx		; Save BX
			push	cx		; Save CX
			push	ax		; Save AX
			mov	bl, al
			mov	al, 'a'
			cmp	bl, al		; Is Character < 'a'
			jl	.notlc		; Other
			mov	al, 'z'
			cmp 	bl, al		; Is Character > 'z'
			jg	.notlc		; Other
			mov	al, 20h
			sub	bl, al		; Convert to Uppercase
			xchg	al, bl		; Exchange AL and BL
			jmp	.lc		; Lowercase Processed
		.notlc:
			pop	ax		; Retrieve AX
			jmp	.clear1
		.lc:
			pop	cx		; Waste AX
		.clear1:
			pop	cx		; Retrieve CX
			pop	bx		; Retrieve BX
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Character To Lowercase		+
; +++++++++++++++++++++++++++++++++++++++

	chr_to_lower:
		
			push	bx		; Save BX
			push	cx		; Save CX
			push	ax		; Save AX
			mov	bl, al
			mov	al, 'A'
			cmp	bl, al		; Is Character < 'A'
			jl	.notuc			; Other
			mov	al, 'Z'
			cmp 	bl, al		; Is Character > 'Z'
			jg	.notuc			; Other
			mov	al, 20h
			add	bl, al		; Convert to Lowercase
			xchg	al, bl		; Exchange AL and BL
			jmp	.uc		; Uppercase Processed
		.notuc:
			pop	ax		; Retrieve AX
			jmp	.clear2
		.uc:
			pop	cx		; Waste AX
		.clear2:
			pop	cx		; Retrieve CX
			pop	bx		; Retrieve BX
			ret

	
; +++++++++++++++++++++++++++++++++++++++
; +	String To Uppercase		+
; +++++++++++++++++++++++++++++++++++++++

	str_to_upper:
		
			push	si			; Save SI
			push	ax			; Save AX
		Count1:
			mov	al, [si]
			cmp	al, 0h			; Check If AL=0
			jz	Count1Done		; AL=0? Then Return
			mov	al, 'a'
			cmp	[si], al		; Is Character < 'a'
			jl	.Other1			; Other
			mov	al, 'z'
			cmp 	[si], al		; Is Character > 'z'
			jg	.Other1			; Other
			mov	al, 20h
			sub	[si], al		; Convert to Uppercase
		.Other1:
			inc	si			; Increment SI
			jmp	Count1
		Count1Done:
			pop	ax			; Retrieve AX
			pop	si			; Retrieve SI
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	String To Lowercase		+
; +++++++++++++++++++++++++++++++++++++++

	str_to_lower:
		
			push	si			; Save SI
			push	ax			; Save AX
		Count2:
			mov	al, [si]
			or	al, al			; Check If AL=0
			jz	Count2Done		; AL=0? Then Return
			mov	al, 'A'
			cmp	[si], al		; Is Character < 'A'
			jl	.Other2			; Other
			mov	al, 'Z'
			cmp 	[si], al		; Is Character > 'Z'
			jg	.Other2			; Other
			mov	al, 20h
			add	[si], al		; Convert to Lowercase
		.Other2:
			inc	si			; Increment SI
			jmp	Count2
		Count2Done:
			pop	ax			; Retrieve AX
			pop	si			; Retrieve SI
			ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Produce Sound In Buzzer		+
; +++++++++++++++++++++++++++++++++++++++

	sound:
		push	ax
		push	cx
		mov	cx, ax			; Temporarily Save Note Value
		mov	al, 182
		out	43h, al
		mov	ax, cx			; Set up frequency
		out	42h, al
		mov	al, ah
		out	42h, al
		in	al, 61h			; Switch PC speaker on
		or	al, 03h
		out	61h, al
		pop	cx
		pop	ax
		ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Produce Delay In Milliseconds	+
; +++++++++++++++++++++++++++++++++++++++

	delay:
	os_pause:
		push	ax
		cmp	ax, 0
		je	.time_up			; If delay = 0 then bail out

		mov	cx, 0
		mov	[.counter_var], cx		; Zero the counter variable

		mov	bx, ax
		mov	ax, 0
		mov	al, 1				; 1 * 55ms = 55mS
		mul	bx				; Multiply by number of 55ms chunks required 
		mov	[.orig_req_delay], ax		; Save it

		mov	ah, 0
		int	1Ah				; Get tick count	

		mov	[.prev_tick_count], dx		; Save it for later comparison

	.checkloop:
		mov	ah,0
		int	1Ah				; Get tick count again

		cmp	[.prev_tick_count], dx		; Compare with previous tick count

		jne	.up_date			; If it's changed check it    		
		jmp	.checkloop			; Otherwise wait some more

	.time_up:
		pop	ax
		ret

	.up_date:
		mov	ax, [.counter_var]		; Inc counter_var
		inc	ax
		mov	[.counter_var], ax

		cmp	ax, [.orig_req_delay]		; Is counter_var = required delay?
		jge	.time_up			; Yes, so bail out

		mov	[.prev_tick_count], dx		; No, so update .prev_tick_count 

		jmp	.checkloop			; And go wait some more


	.orig_req_delay		dw	0
	.counter_var		dw	0
	.prev_tick_count	dw	0

	

; +++++++++++++++++++++++++++++++++++++++
; +	Mute Buzzer			+
; +++++++++++++++++++++++++++++++++++++++

	mute:
		push	ax
		in 	al, 61h
		and 	al, 0FCh
		out 	61h, al
		pop	ax
		ret
	

; +++++++++++++++++++++++++++++++++++++++
; +	Produce Beep			+
; +++++++++++++++++++++++++++++++++++++++

	beep:
		push	ax
		mov	ax, 560d	; Sound Tone
		call	sound
		xor	ax, ax
		mov	ax, 02h		; 110 milliseconds
		call	delay
		call	mute
		pop	ax
		ret
	