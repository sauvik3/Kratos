; ###############################################
; #		Kratos Version : 2.0		#
; ###############################################
;

;
; A20 and C Startup Code...
;


bits	16
org	0x0500

jmp	main

;*******************************************************
;	Preprocessor directives
;*******************************************************

%include "C:\Users\admin\Desktop\new_code\osdev\SANOS_IDA\bootcd\SRC\sys\stdio.inc"			; basic i/o routines
%include "C:\Users\admin\Desktop\new_code\osdev\SANOS_IDA\bootcd\SRC\sys\Gdt.inc"			; Gdt routines
%include "C:\Users\admin\Desktop\new_code\osdev\SANOS_IDA\bootcd\SRC\sys\A20.inc"			; A20 enabling
%include "C:\Users\admin\Desktop\new_code\osdev\SANOS_IDA\bootcd\SRC\sys\common.inc"

;*******************************************************

main:
	cli				; Clear Interrupts
	xor	ax, ax
	mov	ds, ax
	mov	es, ax
	mov	ax, 0x0			; stack begins at 0x9000-0xffff
	mov	ss, ax
	mov	sp, 0xFFFF
	sti				; enable interrupts

	mov	si, LoadingMsg
	call	Puts16

	call	InstallGDT		; install GDT
	call	EnableA20		; Enable A20 Line through Keyboard Controller

	;-------------------------------;
	;   Go into pmode		;
	;-------------------------------;

EnterStage3:

	cli				; clear interrupts
	mov	eax, cr0		; set bit 0 in cr0--enter pmode
	or	eax, 1
	mov	cr0, eax

	jmp	CODE_DESC:Stage3	; far jump to fix CS. Remember that the code selector is 0x8!

	; Note: Do NOT re-enable interrupts! Doing so will triple fault!
	; We will fix this in Stage 3.

;******************************************************
;	ENTRY POINT FOR STAGE 3
;******************************************************

bits 32

Stage3:

	;-------------------------------;
	;   Set registers		;
	;-------------------------------;

	mov	ax, DATA_DESC	; set data segments to data selector (0x10)
	mov	ds, ax
	mov	ss, ax
	mov	es, ax
	mov	esp, 90000h		; stack begins from 90000h

	call	ClrScr32
	mov	ebx, msg
	call	Puts32

	cli				; Disable Interrupts
	hlt				; Halt the System
;--------------------------------------------------------------------------

; Strings

LoadingMsg db 0x0D, 0x0A, "_Starting Operating System Loader...", 0x0D, 0x0A, 0x00
msg db  0x0A, 0x0A, "                       - KRATOS VERSION 2.0 -"
    db  0x0A, 0x0A, "                       32 Bit Loader Executing", 0x0A, 0
