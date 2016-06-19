;**********************************************************************;
;* Description    : represents the assembler interface to a           *;
;*                  C program which can be activated by a hotkey      *;
;*                  as a TSR program.                                 *;
;*                                                                    *;
;*    Author         : Michael W. Maher                               *;
;*    developed on   : 11/10/1990                                     *;
;*    last update    : 11/20/1990                                     *;
;*--------------------------------------------------------------------*;
;*    to assemble    : MASM TSRCA;                                    *;
;*                     ... combine with C program use make file.      *;
;**********************************************************************;

IGROUP group _text                ;combination of program segments
DGROUP group const,_bss,  _data   ;combination of data segments
       assume CS:IGROUP, DS:DGROUP, ES:DGROUP, SS:DGROUP

CONST  segment word public 'CONST';this segment holds all read-only 
CONST  ends                       ;constants

_BSS   segment word public 'BSS'  ;this segment stores all uninitialized
_BSS   ends                       ;static variables

_DATA  segment word public 'DATA' ;all initialized and global static
                                  ;variables are stored in this
                                  ;segment

extrn  __psp : word               ;segment addr of the PSP of the C prg

_DATA  ends

; Constants
MAX_ID_LEN equ 16                 ;maximum length of the ID string
TC_STACK   equ 512                ;512 bytes are reserved for the stack
                                  ;with TURBO-C

; Program
_TEXT  segment byte public 'CODE' ;the program segment

; Reference to external (C) functions --------------------------------

extrn      _sbrk:near             ;returns end address of the heap

; Public declarations of internal functions --------------------------

public     _tsr_init              ;allows call from C program
public     _is_inst
public     _uninst


; Variables for the interrupt handler --------------------------------
; (only accessible via the code segment) -----------------------------

id_buf     db (MAX_ID_LEN + 1) dup (0)  ;buffer for the ID string
ce_ptr     equ this dword         ;points to the routine CALL_END
ce_ofs     dw offset call_end     ;in the already-installed TSR program
ce_seg     dw ?

; Variables needed for activation of the C program -------------------

c_ss       dw 0                   ;C stack segment
c_sp       dw 0                   ;C stack pointer
c_ds       dw 0                   ;C data segment
c_es       dw 0                   ;C extra segment

c_dta_ofs  dw 0                   ;DTA address of the C program
c_dta_seg  dw 0

c_psp      dw 0                   ;segment addr of the PSP of the C prg
break_adr  dw 0                   ;break address of the heap
fkt_adr    dw 0                   ;address of the C TSR function

; Variables for testing for the hotkey

key_count  dw 0                   ;counter for number of keys pressed
key_max    dw 0                   ;key_max mark for BIOS keyboard flag
recur      db 0                   ;prevents recursive TSR calls
in_bios    db 0                   ;shows activity of the BIOS disk interrupt


daptr      equ this dword         ;pointer to the DOS Indos flag
daptr_ofs  dw 0                   ;offset address
daptr_seg  dw 0                   ;segment address

; The following variables store the old addresses of the interrupt ---
; handler, which will be replaced by the new interrupt handler     ---

int9_ptr   equ this dword         ;old interrupt vector 9h
int9_ofs   dw 0                   ;offset address of the old handler
int9_seg   dw 0                   ;segment address of the old handler

int13_ptr  equ this dword         ;old interrupt vector 13h
int13_ofs  dw 0                   ;offset address of the old handler
int13_seg  dw 0                   ;segment address of the old handler

int28_ptr  equ this dword         ;old interrupt vbector 28h
int28_ofs  dw 0                   ;offset address of the old handler
int28_seg  dw 0                   ;segment address of the old handler

;-- Variables which store the information of the interrupted program.

u_dta_ofs  dw 0                   ;DTA address of interrupted program
u_dta_seg  dw 0

u_psp      dw 0                   ;segment addr of the PSP of int. prg.

uprg_ss    dw 0                   ;SS and SP of the interrupted prg.
uprg_sp    dw 0

;-----------------------------------------------------------------------
; TSR_INIT: ends the C program and makes the new interrupt -----------
;           interrupt handler active
; Call from C: void tsr_init(bool TC,
;                            void (fkt *)(void),
;                            unsigned key_max,
;                            unsigned heap_byte,
;                            char * id_string );

_tsr_init  proc    near

sframe0    struc                  ;structure for accessing the stack
bp0        dw ?                   ;stores BP
ret_adr0   dw ?                   ;return address
tc0        dw ?                   ;compiler (1 = TURBO-C, 0 = MSC )
fktptr0    dw ?                   ;pointer to C TSR function
keymax0    dw ?                   ;max number of keys before calling TSR
heap0      dw ?                   ;heap bytes required
idptr0     dw ?                   ;pointer to the ID string
sframe0    ends                   ;end of the structure

frame      equ [ bp - bp0 ]

           push bp                ;store BP on the stack
           mov  bp,sp             ;move SP to BP

           ;-- save the C segment registers ----------------------------

           mov  cs:c_ss,ss        ;store the registers in the 
           mov  cs:c_sp,sp        ;corresponding variables
           mov  cs:c_es,es
           mov  cs:c_ds,ds

           ;-- copy the ID string into the internal buffer -------------

           mov  si,frame.idptr0    ;DS:SI now points to the string
           push cs                 ;move CS to the stack
           pop  es                 ;and restore as ES
           mov  di,offset id_buf   ;ES:DI now points to ID_BUF
           mov  cx,MAX_ID_LEN      ;copy maxmimum of MAX_ID_LEN chars
ti0:       lodsb                   ;get character from string
           stosb                   ;and place in internal buffer
           or   al,al              ;test for end of string
           loopne ti0              ;continue if char!=0 and CX!=0

           ;-- store the parameters passed ----------------------------

           mov  ax,frame.fktptr0  ;get pointer to the C TSR function
           mov  cs:fkt_adr,ax     ;and save
	   mov  ax,frame.keymax0  ;get max number of keys
	   mov  cs:key_max,ax     ;and save


           ;-- determine DTA address of the C program ------------------

           mov  ah,2fh            ;ftn. no.: get DTA address
           int  21h               ;call DOS interrupt
           mov  cs:c_dta_ofs,bx   ;store address in the corresponding
           mov  cs:c_dta_seg,es   ;variables

           ;-- determine address of the INDOS flag ---------------------

           mov  ah,34h            ;ftn. no.: get addr of the INDOS flag
           int  21h               ;call DOS interrupt
           mov  cs:daptr_ofs,bx   ;save address in the corresponding
           mov  cs:daptr_seg,es   ;variables

           ;-- get the addresses of the interrupt handler  -------------

           mov  ax,3509h          ;get interrupt vector 9h
           int  21h               ;call DOS interrupt
           mov  cs:int9_ofs,bx    ;save address of the handler in the 
           mov  cs:int9_seg,es    ;appropriate variable

           mov  ax,3513h          ;get interrupt vector 13h
           int  21h               ;call DOS interrupt
           mov  cs:int13_ofs,bx   ;store address of the handler in the
           mov  cs:int13_seg,es   ;corresponding variables

           mov  ax,3528h          ;get interrupt vector 28h
           int  21h               ;call DOS interrupt
           mov  cs:int28_ofs,bx   ;store address of the handler in the
           mov  cs:int28_seg,es   ;corresponding variables

           ;-- install the new interrupt handlers ---------------------

           push ds                ;save data segment
           mov  ax,cs             ;CS to AX and then load into DS
           mov  ds,ax

           mov  ax,2509h          ;ftn. no.: set interrupt 9h
           mov  dx,offset int09   ;DS:DX stores the addr of the handler
           int  21h               ;call DOS interrupt

           mov  ax,2513h          ;ftn. no.: set interrupt 13h
           mov  dx,offset int13   ;DS:DX stores the addr of the handler
           int  21h               ;call DOS interrupt

           mov  ax,2528h          ;ftn. no.: set interrupt 28h
           mov  dx,offset int28   ;DS:DX stores the addr of the handler
           int  21h               ;call DOS interrupt

           pop  ds                ;restore DS from stack

           ;-- calculatre number of paragraphs which must remain -------
           ;-- in memory.                                        -------

           xor  ax,ax             ;determine current break address
           push ax                ;as argument for SBRK on the stack
           call _sbrk             ;call C function SBRK
                                  ;AX contains the end addr of the heap
           pop  cx                ;get argument from stack again
           add  ax,frame.heap0    ;add required heap memory

           ;-- With TURBO-C the stack is found behind the heap and -----
           ;-- begins with the end of the segment. It must thus
           ;-- be moved near the heap.

           cmp  byte ptr frame.tc0,0  ;using TURBO-C?
           je   msc               ;no, MSC

           add  ax,TC_STACK-1     ;calculate new stack pointer for TC
           mov  cs:c_sp,ax        ;and store
           inc  ax                ;set break address

           ;-- Calculate number of paragraphs which must remain --------
           ;-- resident in memory.

msc:       mov  dx,ax             ;get break address into DX
           add  dx,15             ;avoid loss through integer division
           mov  cl,4              ;shift 4 times to the right and then
           shr  dx,cl             ;divide by 16
           mov  ax,ds             ;move AX to DS
           mov  bx,__psp          ;get segment address of the PSP
           mov  cs:c_psp,bx       ;save in a variable
           sub  ax,bx             ;subtract DS from PSP
           add  dx,ax             ;and add to the number of paragraphs
           mov  ax,3100h          ;ftn. no.: end resident program
           int  21h               ;call DOS interrupt and end program

_tsr_init  endp

;-----------------------------------------------------------------------
; IS_INST: determines if the program is already installed ------------
; Call from C : int ist_inst( char * id_string );
; Return value: 1, if the program was already installed, else 0

_is_inst   proc    near

sframe1    struc                  ;structure for accessing the stack
bp1        dw ?                   ;hold BP
ret_adr1   dw ?                   ;return address
idptr1     dw ?                   ;pointer to the ID string
sframe1    ends                   ;end of the structure
frame      equ [ bp - bp1 ]

           push bp                ;save BP on the stack
           mov  bp,sp             ;move SP to BP
           push di                ;save DI on the stack
           push si                ;save SI on the stack
           push es                ;save ES on the stack

           ;-- determine segment address of the current int 9 handler --

           mov  ax,3509h          ;get interrupt vector 9h
           int  21h               ;DOS interrupt puts seg addr in ES
           mov  di,offset id_buf  ;ES:DI points to installed ID_BUF
           mov  si,frame.idptr1   ;DS:SI points to the ID_STRING passed

           mov  cx,0              ;return code: not installed
isi0:      lodsb                  ;load character from the string
           cmp  al,es:[di]        ;compare to other string
           jne  not_inst          ;not equal --> NOT_INST
           inc  di                ;increment pointer in String2
           or   al,al             ;end of string reached?
           jne  isi0              ;no, keep comparing --> ISI0

           mov  cl,1              ;yes --> the program is installed

not_inst:  mov  ax,cx             ;get return code from ax
           pop  es                ;restore saved registers from stack
           pop  si                
           pop  di
           pop  bp
           ret                    ;back to the caller

_is_inst   endp                   ;end of the procedure

;-----------------------------------------------------------------------
; CALL_END: calls the end function on reinstallation of the TSR program.
; Input   : DI = offset address of the routine to be called
; Info    : This function is not intended to be called by a C program.

call_end   proc far

           call di                ;call the end function
           ret                    ;back to the caller

call_end   endp

;----------------------------------------------------------------------------
; UNINST: reinstalls the TSR program and releases the allocated memory again.
; Call from C : void uninst( void (endfkt *) ( void ) );
; Info        : if the value -1 (0xffff) is passed as the pointer to
;               the end function, no end function will be called.
; Note        : This function should be called only when a prior call
;               to IS_INST() has returned the value 1.

_uninst    proc    near

sframe2    struc                  ;structure for accessing the stack
bp2        dw ?                   ;stores BP
ret_adr2   dw ?                   ;return address
ftnptr2    dw ?                   ;pointer to the end function
sframe2    ends                   ;end of the structure

frame      equ [ bp - bp2 ]

           assume  es:IGROUP      ;allow access to the CS variables
                                  ;via ES

           push bp                ;save BP on the stack
           mov  bp,sp             ;move SP to BP
           push di                ;store DI on the stack
           push si                ;store SI on the stack
           push ds                ;store DS on the stack
           push es                ;store ES on the stack

           ;-- determine the seg addr of the current int 9 handler ---
           mov  ax,3509h          ;get interrupt vector 9h
           int  21h               ;DOS interrupt puts seg addr in ES

           mov  di,frame.ftnptr2  ;get address of the end function
           cmp  di,0ffffh         ;no end function called?
           je   no_endftn         ;NO ---> NO_ENDFTN

           ;-- Perform context switch to C program and execute -------
           ;-- the specified end funtion

           mov  cs:ce_seg,es      ;save ES in jump vector

           mov  cs:uprg_ss,ss     ;save current stack segment and 
           mov  cs:uprg_sp,sp     ;stack pointer

           cli                    ;allow no more interrupts
           mov  ss,es:c_ss        ;activate the stack of the TSR
           mov  sp,es:c_sp        ;program
           sti                    ;allow interrupts again

           push es                ;save ES on the stack
           mov  ah,2fh            ;ftn. no.: get DTA address
           int  21h               ;call DOS interrupt
           mov  cs:u_dta_ofs,bx   ;save address of the DTA of the 
           mov  cs:u_dta_seg,es   ;interrupted program
           pop  es                ;get ES back from the stack

           mov  ah,50h            ;ftn. no.: set address of the PSP
           mov  bx,es:c_psp       ;get seg addr of the PSP of the C prg
           int  21h               ;call DOS interrupt

           push ds                ;save ES and DS on the stack
           push es

           mov  ah,1ah            ;ftn. no.: set DTA address
           mov  dx,es:c_dta_ofs   ;get offset address of the new DTA
           mov  ds,es:c_dta_seg   ;and segment address of the new DTA
           int  21h               ;call DOS interrupt

           mov  ds,es:c_ds        ;set segment register for the
           mov  es,es:c_es        ;C program
           call cs:[ce_ptr]       ;call the function

           ;-- perform context change to the interrupt program -------

           mov  ah,1ah            ;ftn. no.: set DTA address
           mov  dx,cs:u_dta_ofs   ;load offset and segment address of
           mov  ds,cs:u_dta_seg   ;the interrupted program
           int  21h               ;call DOS interrupt

           pop  es                ;seg addr of the TSR prog from stack
           pop  ds                ;restore DS from stack
           mov  ah,50h            ;ftn. no.: set address of the PSP
           mov  bx,__psp          ;load seg addr of the PSP
           int  21h               ;call DOS interrupt

           cli                    ;don't allow interrupts
           mov  ss,cs:uprg_ss     ;restore stack pointer and stack
           mov  sp,cs:uprg_sp     ;segment
           sti                    ;allow interrupts again

           ;-- reinstall the interrupt handler of the TSR --------------
           ;-- program                                    --------------

no_endftn: cli                    ;don't allow interrupts
           mov  ax,2509h          ;ftn. no.: set handler for int 9
           mov  ds,es:int9_seg    ;segment address of the old handler
           mov  dx,es:int9_ofs    ;offset address of the old handler
           int  21h               ;install the old handler again

           mov  ax,2513h          ;ftn. no.: set handler for int 13
           mov  ds,es:int13_seg   ;segment address of the old handler
           mov  dx,es:int13_ofs   ;offset address of the old handler
           int  21h               ;reinstall the old handler

           mov  ax,2528h          ;ftn. no.: set handler for int 28
           mov  ds,es:int28_seg   ;segment address of the old handler
           mov  dx,es:int28_ofs   ;offset address of the old handler
           int  21h               ;reinstall the old handler

           sti                    ;allow interrupts again

           mov  es,es:c_psp       ;seg addr of the PSP of the TSR prg
           mov  cx,es             ;save in CX
           mov  es,es:[ 02ch ]    ;get seg addr of environment from PSP
           mov  ah,49h            ;ftn. no.: release allocated memory
           int  21h               ;call DOS interrupt

           mov  es,cx             ;restore ES from CX
           mov  ah,49h            ;ftn. no.: release allocated memoru
           int  21h               ;call DOS interrupt

           pop  es                ;get the saved registers back from
           pop  ds                ;the stack
           pop  si
           pop  di

           pop  bp
           ret                    ;back to the called

           assume es:DGROUP       ;combine ES with DGROUP again

_uninst    endp                   ;end of the procedure

;-----------------------------------------------------------------------
;-- The new interrupt routine follows ----------------------------------
;-----------------------------------------------------------------------
;-- The new interrupt 09h handler --------------------------------------

int09      proc far

           pushf                  ;simulate the call of the old handler
           call cs:int9_ptr       ;via the INT 9h instruction

           cli                    ;suppress interrupts
           cmp  cs:recur,0        ;is the TSR prog already active?
           jne  ik_end            ;YES: back to the called of int 9

           ;-- test to see if the BIOS disk int is being executed now

           cmp  cs:in_bios,0      ;BIOS disk interrupt active?
           jne  ik_end            ;yes --> back to the caller

	   ;-- by m.w.maher increment the key-counter -------------------
	   inc  key_count

ik_end:    iret                   ;back to the interrupted program

int09      endp

;-- the new interrupt 13h handler --------------------------------------
int13      proc far

           mov  cs:in_bios,1      ;set flag and show that the BIOS disk
                                  ;interrupt is active
           pushf                  ;call the old interrupt handler
           call cs:int13_ptr      ;simulate via int 13h
           mov  cs:in_bios, 0     ;BIOS disk interrupt no longer active
	   ret  2                 ;back to the caller, but don't remove
                                  ;the flag reg from the stack first
int13      endp

;-- the new interrupt 28h handler -------------------------------------
int28      proc far

           pushf                  ;simulate calling the old interrupt
           call cs:int28_ptr      ;handler via int 28h

           cli                    ;suppress further interrupts
           cmp  cs:recur,0        ;is the TSR program already active?
           je   id01              ;NO ---> ID01
id_end:    iret                   ;YES   ---> back to the caller

	   ;-- the TSR program is not yet active ------------------------

id01:      cmp  cs:in_bios, 0     ;BIOS disk interrupt active?
           jne  id_end            ;YES --> back to the caller

	   ;-- m.w.maher BIOS disk interrupt not active, test key_count -
	   push ax
	   mov  ax,cs:key_max
	   cmp  cs:key_count,ax   ;compare key_count to key_max
	   pop  ax
	   jl   ik_end            ;key_count >= MAX_KEYS? NO --> back

	   mov  key_count, 0
           call  start_tsr        ;start the TSR program
           iret                   ;back to the interrupted program

int28      endp

;-- START_TSR: activate the TSR program ---------------------------------
start_tsr  proc near

           mov  cs:recur,1        ;set TSR recursion flag


           ;-- perform context change to the C program -----------------

           mov  cs:uprg_ss,ss     ;save current stack segment and
           mov  cs:uprg_sp,sp     ;stack pointer

           mov  ss,cs:c_ss        ;activate the C program's stack
           mov  sp,cs:c_sp

           push ax                ;save the processor registers on the
           push bx                ;C stack
           push cx
           push dx
           push bp
           push si
           push di
           push ds
           push es

           ;-- save 64 words from the DOS stack ------------------------

           mov  cx,64             ;loop counter
           mov  ds,cs:uprg_ss     ;set DS:SI to the end of the DOS stack
           mov  si,cs:uprg_sp

tsrs1:     push word ptr [si]     ;save word from the DOS stack to the
           inc  si                ;C stack and set SI to the next
           inc  si                ;stack word
           loop tsrs1             ;process all 64 words

           mov  ah,51h            ;ftn. no.: determine address of PSP
           int  21h               ;call DOS interrupt
           mov  cs:u_psp,bx       ;save segment address of the PSP

           mov  ah,2fh            ;ftn. no.: get DTA address
           int  21h               ;call DOS interrupt
           mov  cs:u_dta_ofs,bx   ;store address of the DTA of the
           mov  cs:u_dta_seg,es   ;interrupted program

           mov  ah,50h            ;ftn. no.: set address of the PSP
           mov  bx,cs:c_psp       ;get seg addr of the PSP of the C prg
           int  21h               ;call DOS interrupt

           mov  ah,1ah            ;ftn. no.: set DTA address
           mov  dx,cs:c_dta_ofs   ;get offset address of the new DTA
           mov  ds,cs:c_dta_seg   ;and the segment address of new DTA
           int  21h               ;call DOS interrupt

           mov  ds,cs:c_ds        ;set segment register for the C
           mov  es,cs:c_es        ;program

           sti                    ;allow interrupts again
           call cs:fkt_adr        ;call the start function of the C prg.
           cli                    ;disable interrupts


           ;-- perform context change to the interrupted program -------

           mov  ah,1ah            ;ftn. no.: set DTA address
           mov  dx,cs:u_dta_ofs   ;load offset and segment addresses
           mov  ds,cs:u_dta_seg   ;of the DTA of the interrupted program
           int  21h               ;call DOS interrupt

           mov  ah,50h            ;ftn. no.: set address of the PSP
           mov  bx,cs:u_psp       ;seg addr PSP of the interrupted prg.
           int  21h               ;call DOS interrupt

           ;-- restore DOS stack again --------------------------------

           mov  cx,64             ;loop counter
           mov  ds,cs:uprg_ss     ;load DS:SI with the end address of 
           mov  si,cs:uprg_sp     ;the DOS stack
           add  si,128            ;set SI to the start of the DOS stack
tsrs2:     dec  si                ;SI to the previous stack word
           dec  si
           pop  word ptr [si]     ;get word from the C stack to DOS stack
           loop tsrs2             ;process all 64 words

           pop  es                ;restore the saved registers from the
           pop  ds                ;C stack
           pop  di
           pop  si
           pop  bp
           pop  dx
           pop  cx
           pop  bx
           pop  ax

           mov  ss,cs:uprg_ss     ;reset stack pointer and stack segment
           mov  sp,cs:uprg_sp     ;of the interrupted program

           mov  cs:recur,0        ;reset TSR recursion flag
           ret                    ;back to the caller

start_tsr  endp

;-----------------------------------------------------------------------
_text      ends                   ;end of the code segment
           end                    ;end of the program

