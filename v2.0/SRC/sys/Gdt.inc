;*************************************************
;		GDT Routines
;*************************************************

%ifndef __GDT_INC_INCLUDED__
%define __GDT_INC_INCLUDED__

bits	16

;----------------------------------------------------------------------------
InstallGDT:

	cli                  ; clear interrupts
	pusha                ; save registers
	lgdt 	[toc]        ; load GDT into GDTR
	sti	             ; enable interrupts
	popa                 ; restore registers
	ret	             ; All done!
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; The GDT Table
;----------------------------------------------------------------------------

gdt_data: 
	dd 0                ; null descriptor
	dd 0 

; code descriptor

	dw 0FFFFh           ; limit low
	dw 0                ; base low
	db 0                ; base middle
	db 10011010b        ; access
	db 11001111b        ; granularity
	db 0                ; base high

; data descriptor

	dw 0FFFFh           ; limit low
	dw 0                ; base low
	db 0                ; base middle
	db 10010010b        ; access
	db 11001111b        ; granularity
	db 0                ; base high

end_of_gdt:
toc: 
	dw end_of_gdt - gdt_data - 1 	; limit (Size of GDT)
	dd gdt_data 			; base of GDT
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; give the descriptor offsets names
;----------------------------------------------------------------------------
%define NULL_DESC 0
%define CODE_DESC 0x8
%define DATA_DESC 0x10
;----------------------------------------------------------------------------

%endif