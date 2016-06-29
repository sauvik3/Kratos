;-------------------------------------------------
; Boot Program.
;-------------------------------------------------

	bits	16	; we are in 16 bit real mode
	org 0x0000	; we will set regisers later

; Execution begins here

entry:
	jmp	begin 	; jump over the DOS boot record data

; -------------------------------------------------
; data portion of the "DOS BOOT RECORD"
; --------------------------------------------------

;brINT13Flag     DB      90H             ; 0002h - 0EH for INT13 AH=42 READ
brOEM           DB      '_SAUVIK_'      ; 0003h - OEM name & DOS version (8 chars)
brBPS           DW      512             ; 000Bh - Bytes/sector
brSPC           DB      1               ; 000Dh - Sectors/cluster
brResCount      DW      1               ; 000Eh - Reserved (boot) sectors
brFATs          DB      2               ; 0010h - FAT copies
brRootEntries   DW      0E0H		; 0011h - Root directory entries
brSectorCount   DW      2880		; 0013h - Sectors in volume, < 32MB
brMedia         DB      240		; 0015h - Media descriptor
brSPF           DW      9               ; 0016h - Sectors per FAT
brSPH           DW      18              ; 0018h - Sectors per track
brHPC           DW      2		; 001Ah - Number of Heads
brHidden        DD      0               ; 001Ch - Hidden sectors
brSectors       DD      0	        ; 0020h - Total number of sectors
brDriveNum	DB      0               ; 0024h - Physical drive no.
		DB      0               ; 0025h - Reserved (FAT32)
		DB      29H             ; 0026h - Extended boot record sig 
brSerialNum     DD      12345678H       ; 0027h - Volume serial number (random)
brLabel         DB      'NO NAME    '   ; 002Bh - Volume label  (11 chars)
brFSID          DB      'FAT12   '      ; 0036h - File System ID (8 chars)
;------------------------------------------------------------------------

; ____________________________________________
;	Prints a string
;	DS=>SI: 0 terminated string
; ____________________________________________

Print:
			lodsb					; load next byte from string from SI to AL
			or			al, al		; Does AL=0?
			jz			PrintDone	; Yep, null terminator found-bail out
			mov			ah,	0eh	; Nope-Print the character
			int			10h
			jmp			Print		; Repeat until null terminator found
PrintDone:
			ret					; we are done, so return

absoluteSector db 0x00
absoluteHead   db 0x00
absoluteTrack  db 0x00

;************************************************;
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;************************************************;

ClusterLBA:
          sub     ax, 0x0002                          ; zero base cluster number
          xor     cx, cx
          mov     cl, BYTE [brSPC]     		      ; convert byte to word
          mul     cx
          add     ax, WORD [datasector]               ; base data sector
          ret

;************************************************;
; Convert LBA to CHS
; AX=>LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;************************************************;

LBACHS:
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [brSPH]           	      ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [brHPC]          	      ; calculate
          mov     BYTE [absoluteHead], dl
          mov     BYTE [absoluteTrack], al
          ret


;************************************************;
; Reads a series of sectors
; CX=>Number of sectors to read
; AX=>Starting sector
; ES:BX=>Buffer to read to
;************************************************;

ReadSectors:
     .MAIN:
          mov     di, 0x0005                          ; five retries for error
     .SECTORLOOP:
          push    ax
          push    bx
          push    cx
          call    LBACHS                              ; convert starting sector to CHS
          mov     ah, 0x02                            ; BIOS read sector
          mov     al, 0x01                            ; read one sector
          mov     ch, BYTE [absoluteTrack]            ; track
          mov     cl, BYTE [absoluteSector]           ; sector
          mov     dh, BYTE [absoluteHead]             ; head
          mov     dl, BYTE [brDriveNum]		      ; drive
          int     0x13                                ; invoke BIOS
          jnc     .SUCCESS                            ; test for read error
          xor     ax, ax                              ; BIOS reset disk
          int     0x13                                ; invoke BIOS
          dec     di                                  ; decrement error counter
          pop     cx
          pop     bx
          pop     ax
          jnz     .SECTORLOOP                         ; attempt to read again
          int     0x18
     .SUCCESS:
          mov     si, msgProgress
          call    Print
          pop     cx
          pop     bx
          pop     ax
          add     bx, WORD [brBPS]        	      ; queue next buffer
          inc     ax                                  ; queue next sector
          loop    .MAIN                               ; read next sector
          ret


; --------------------------------------------
;  Boot program code begins here
; --------------------------------------------

begin:
     
     ;----------------------------------------------------
     ; code located at 07C0:0000, adjust segment registers
     ;----------------------------------------------------
     
          cli						; disable interrupts
          mov     ax, 0x07C0				; setup registers to point to our segment
          mov     ds, ax
          mov     es, ax
          mov     fs, ax
          mov     gs, ax

     ;----------------------------------------------------
     ; create stack
     ;----------------------------------------------------
     
          mov     ax, 0x0000				; set the stack
          mov     ss, ax
          mov     sp, 0xFFFF
          sti						; restore interrupts

          mov  [bootdevice], dl

     ;----------------------------------------------------
     ; Load root directory table
     ;----------------------------------------------------

     LOAD_ROOT:
     
     ; compute size of root directory and store in "cx"

          xor     cx, cx
          xor     dx, dx
          mov     ax, 0x0020                          ; 32 byte directory entry
          mul     WORD [brRootEntries]                ; total size of directory
          div     WORD [brBPS]             	      ; sectors used by directory
          xchg    ax, cx
          
     ; compute location of root directory and store in "ax"
     
          mov     al, BYTE [brFATs] 	                ; number of FATs
          mul     WORD [brSPF]               		; sectors used by FATs
          add     ax, WORD [brResCount]       		; adjust for bootsector
          mov     WORD [datasector], ax                 ; base of root directory
          add     WORD [datasector], cx
          
     ; read root directory into memory (7C00:0200)
     
          mov     bx, 0x0200                            ; copy root dir above bootcode
          call    ReadSectors

     ;----------------------------------------------------
     ; Find stage 2
     ;----------------------------------------------------

     ; browse root directory for binary image
          mov     cx, WORD [brRootEntries]              ; load loop counter
          mov     di, 0x0200                            ; locate first root entry
     .LOOP:
          push    cx
          mov     cx, 0x000B                            ; eleven character name
          mov     si, ImageName                         ; image name to find
          push    di
     rep  cmpsb                                         ; test for entry match
          pop     di
          je      LOAD_FAT
          pop     cx
          add     di, 0x0020                            ; queue next directory entry
          loop    .LOOP
          jmp     FAILURE

     ;----------------------------------------------------
     ; Load FAT
     ;----------------------------------------------------

     LOAD_FAT:
     
     ; save starting cluster of boot image
     
          mov     dx, WORD [di + 0x001A]
          mov     WORD [cluster], dx                  ; file's first cluster
          
     ; compute size of FAT and store in "cx"
     
          xor     ax, ax
          mov     al, BYTE [brFATs]        ; number of FATs
          mul     WORD [brSPF]             ; sectors used by FATs
          mov     cx, ax

     ; compute location of FAT and store in "ax"

          mov     ax, WORD [brResCount]       ; adjust for bootsector
          
     ; read FAT into memory (7C00:0200)

          mov     bx, 0x0200                          ; copy FAT above bootcode
          call    ReadSectors

     ; read image file into memory (0050:0000)
     
          mov     ax, 0x0050
          mov     es, ax                              ; destination for image
          mov     bx, 0x0000                          ; destination for image
          push    bx

     ;----------------------------------------------------
     ; Load Stage 2
     ;----------------------------------------------------

     LOAD_IMAGE:
     
          mov     ax, WORD [cluster]                  ; cluster to read
          pop     bx                                  ; buffer to read into
          call    ClusterLBA                          ; convert cluster to LBA
          xor     cx, cx
          mov     cl, BYTE [brSPC]     		      ; sectors to read
          call    ReadSectors
          push    bx
          
     ; compute next cluster
     
          mov     ax, WORD [cluster]                  ; identify current cluster
          mov     cx, ax                              ; copy current cluster
          mov     dx, ax                              ; copy current cluster
          shr     dx, 0x0001                          ; divide by two
          add     cx, dx                              ; sum for (3/2)
          mov     bx, 0x0200                          ; location of FAT in memory
          add     bx, cx                              ; index into FAT
          mov     dx, WORD [bx]                       ; read two bytes from FAT
          test    ax, 0x0001
          jnz     .ODD_CLUSTER
          
     .EVEN_CLUSTER:
     
          and     dx, 0000111111111111b               ; take low twelve bits
         jmp     .DONE
         
     .ODD_CLUSTER:
     
          shr     dx, 0x0004                          ; take high twelve bits
          
     .DONE:
     
          mov     WORD [cluster], dx                  ; store new cluster
          cmp     dx, 0x0FF0                          ; test for end of file
          jb      LOAD_IMAGE
          
     DONE:
     
          mov     si, msgCRLF
          call    Print
	  mov	  dl, [bootdevice]
          push    WORD 0x0050
          push    WORD 0x0000
          retf
          
     FAILURE:
     
          mov     si, msgFailure
          call    Print
          mov     ah, 0x00
          int     0x16                                ; await keypress
          int     0x19                                ; warm boot computer

;---------------------------------------------

bootdevice  db 0
datasector  dw 0x0000
cluster     dw 0x0000
ImageName   db "KRATOS  SYS"
msgCRLF     db 0x0D, 0x0A, 0x00
msgProgress db ".", 0x00
msgFailure  db 0x0D, 0x0A, "Press Any Key to Reboot...", 0x0D, 0x0A, 0x00

size	equ	$ - entry
%if size+2 > 512
  %error "Code too large for boot-sector"
%endif
	times	(512 - size - 2) db 0

	db	0x55, 0xAA		;2  byte boot signature