;
; project.asm
;
; Created: 20/05/2017 07:48:47
; Author : Edward & Gary 
;

.include "m2560def.inc"

.def temp = r16
.def temp1 = r17
.def temp2 = r18
.def digit = r19
.def counter = r20
.def lcd = r21
.def waitingFlag = r22
.def debounceFlag = r23
.def col = r24
.def row = r25
.def cmask = r26
.def rmask = r27
.equ ODDEVENMASK = 0x01
.equ PORTLDIR = 0xF0        ; PH7-4: output, PH3-0, input
.equ INITCOLMASK = 0xEF     ; scan from the rightmost column,
.equ INITROWMASK = 0x01     ; scan from the top row
.equ ROWMASK = 0x0F         ; for obtaining input from Port L
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro do_lcd_command
	ldi lcd, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro
.macro do_lcd_data
	ldi lcd, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro do_lcd_rdata
	mov lcd, @0
	subi lcd, -'0'
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro
.macro clear
	ldi YL, low(@0)		; load the memory address to Y
	ldi YH, high(@0)
	clr temp
	st Y+, temp			; clear the two bytes at @0 in SRAM
	st Y, temp
.endmacro
.macro defitem
	.db @0, @1
	.set T = PC
.endmacro

.dseg
OC: .byte 2						; One Second Counter   
HC:	.byte 2						; Half Second Counter
DC: .byte 2						; Debounce Counter
RC: .byte 1						; Return Coin
NP: .byte 1						; Number Pressed
RCPATTERN: .byte 1
PR: .byte 1						; Price
QUANTITY: .byte 9

.cseg
.org 0x0000
	jmp RESET

.org INT0addr
   jmp PB0_Interrupt

.org INT1addr
   jmp PB1_Interrupt

.org OVF0addr
	jmp Timer0OVF ; Jump to the interrupt handler for
					; Timer0 overflow
/*
.org ADCCaddr
	jmp POT_Interrupt
*/
defitem "1",  "4"  ;9  coin  quantity
defitem "2",  "3"  ;8
defitem "1",  "0"  ;7
defitem "2",  "8"  ;6
defitem	"1",  "2"  ;5
defitem "2",  "9"  ;4
defitem "1",  "4"  ;3
defitem "2",  "3"  ;2
defitem "1",  "0"  ;1


RESET:
	ldi YL, low(RAMEND)
	ldi YH, high(RAMEND)
	out SPH, YH
	out SPL, YL				;reset SP
	
	;initilize LED
    ser temp
	out DDRC, temp			;Set port C to output
	clr temp
	out PORTC, temp			;pass the lower pattern to portC
	
	;initialize timer counter
	clear DC
	clear OC

	;initialize PB0 & PB1 button
	clr temp
	out DDRB, temp	
	out PORTB, temp			;Set pott B to input

	;initialize LCD
	ser temp
	out DDRF, temp
	out DDRA, temp
	clr temp
	out PORTF, temp
	out PORTA, temp

	; keypad setup
    ldi temp1, PORTLDIR     ; PB7:4/PB3:0, out/in
    sts DDRL, temp1         ; PORTB is input

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; Cursor on, bar, no blink

	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data '7'
	do_lcd_data 's'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data 'C'
	do_lcd_data '8'
	do_lcd_command 0b11000000
	do_lcd_data 'V'
	do_lcd_data 'e'
	do_lcd_data 'n'
	do_lcd_data 'd'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'M'
	do_lcd_data 'a'
	do_lcd_data 'c'
	do_lcd_data 'h'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'e'

	rjmp main

end:
	rjmp end

;*******************************************************************
;interruption stuff starts

Timer0OVF:
	push temp
	in temp, SREG
	push temp			; Prologue starts.
	push YH				; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24

checkFlagSet:
	cpi waitingFlag, 1		; WF=1 starting screen
	breq oneSecond
	cpi waitingFlag, 2		; out of stock screen: 1.turn the led on
	breq buttonDebounce
	cpi debounceFlag, 1					; WF=0 && DF=1: normal waiting but keypad pressed
	breq keyDebounce
	clear DC

	rjmp Endif

jmpChangeScreen:
	jmp changeScreen

oneSecond:
	cpi digit, 1			; WF=1 starting screen can be interrupt by 
	breq jmpChangeScreen	; pressing keypad
	lds r24, OC
	lds r25, OC+1 
	adiw r25:r24, 1
	cpi r24, low(7812)
	ldi temp, high(7812)
	cpc r25, temp
	brne NotaSecond
	clear OC

countThree:
	inc counter
	cpi counter, 3
	breq isThree
	rjmp Endif

isThree:
	ldi debounceFlag, 1				;incase button not pressed
	rjmp changeScreen

NotaSecond:
	sts OC, r24
	sts OC+1, r25
	rjmp Endif

keyDebounce:
	;out PORTC, debounceFlag
	lds r24, DC
    lds r25, DC+1
    adiw r25:r24, 1			; Increase the temporary counter by one.

    cpi r24, low(2000)		; disable keypad for 50ms
    ldi temp, high(2000)	; DF=1
    cpc temp, r25
    brne notHundred			; 100 milliseconds have not passed
	clear DC
	clr debounceFlag		; renable keypad
    rjmp EndIF

notHundred: 		; Store the new value of the debounce counter.
	sts DC, r24
	sts DC+1, r25
	rjmp Endif

turnOnLED:
	ser temp
	out PORTC, temp

buttonDebounce:
	adiw r31:r30,1			; Everytime i increment DebounceCounter
	cpi r30,low(500)		; Check if the debounceCounter reaches ~80ms, we enables the flag
	ldi temp,high(500)
	cpc r31,temp
	brne halfSecond
	ldi DebounceFlag, 2		;enable button to interrpt the program
halfSecond:
	lds r24, HC
	lds r25, HC+1 
	adiw r25:r24, 1
	cpi r24, low(3906)
	ldi temp, high(3906)
	cpc r25, temp
	brne NotaHalfSecond
	clear HC

flashLED:
	cpi counter, 5
	breq isThree
	inc counter
	mov temp, counter
	andi temp, ODDEVENMASK
	cpi temp, 0			; odd
	breq turnOnLED		
	clr temp			; even
	out PORTC, temp
	rjmp Endif

NotaHalfSecond:
	sts HC, r24
	sts HC+1, r25

Endif:
	pop	r24
	pop	r25
	pop	YL
	pop	YH
	pop	temp
	out SREG, temp
	pop temp
	reti

return:
	reti

PB0_Interrupt:
	cpi debounceFlag, 2				;if the buttons are still debouncing
	brne return						;Do nothing
	push temp
	in temp, SREG
	push temp
	clr r30
	clr r31
	clr temp
	out PORTC, temp
	pop temp
	out SREG, temp
	pop temp
	rjmp changeScreen

PB1_Interrupt:
	cpi debounceFlag, 2
	brne return
	push temp
	in temp, SREG
	push temp
	clr r30
	clr r31
	clr temp
	out PORTC, temp
	pop temp
	out SREG, temp
	pop temp
	rjmp changeScreen

changeScreen:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'S'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'e'
	do_lcd_data 'c'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'
	do_lcd_command 0b11000000	; break to the next line
	rcall sleep_5ms

	; Any time counting should be cleared
	clear OC
	clear HC
	clear DC
	clr counter

	clr waitingFlag				; Back to normal mode
	cpi debounceFlag, 1			; Any key pressed
	breq keepDebounce			; disable keypad input for more 50 ms 
	cpi debounceFlag, 2			; button interrupted
	breq returnClear			; clear debounceFlag

	rjmp Endif

returnClear:					; return button interrupt
	clr debounceFlag
	reti

keepDebounce:					; disable keypad input for more 50 ms
	rjmp keyDebounce			; debounceFlag will be reset in 50ms

;interruption stuff ends
;*****************************************************************************


main:
	; Button PB0 & PB1 initialization
	ldi temp, (1<<ISC01 | 1<<ISC11)	;set failing edge for INT0 and INT1
	sts EICRA, temp
	in temp, EIMSK					
	ori temp, (1<<INT0 | 1<<INT1)	;Enable INT0/1
	out EIMSK, temp

	; general initialization
	clr counter
	clr debounceFlag
	
	;set timer interrupt
	clr temp
	out TCCR0A, temp
	ldi temp, (1<<CS01)
	out TCCR0B, temp		; Prescaling value=8
	ldi temp, 1<<TOIE0		; Enable timeroverflow flag
	sts TIMSK0, temp
	sei						; Enable global interrupt*/

	;initialize Z
	ldi ZH, high(T << 1)
	ldi ZL, low(T << 1)
	sbiw Z, 1
	ldi YH, high(QUANTITY)
	ldi YL, low(QUANTITY)
	push counter
initQuantity:
	;initialize the quantity
	lpm temp,Z
	subi temp, 48
	sbiw Z, 1
	inc counter
	st Y+,temp
	;subi temp, 48  ;price
	cpi counter, 18
	brne initQuantity
	clr r30
	clr r31
	pop counter

	ldi waitingFlag, 1	;initialize the waiting from starting screen

initKeypadClear:
	clr digit
initKeypad:
	;out PORTC, digit
	; waitingFlag check
	cpi waitingFlag, 2		; WF=2 DF=1 out of stock screen
	breq initKeypad
	; debounce check
	cpi debounceFlag, 1		; WF=0 DF=1 key pressed
	breq initKeypad

    ldi cmask, INITCOLMASK  ; initial column mask
    clr col                 ; initial column
	clr temp
	clr temp1
	clr temp2

	

colloop:
    cpi col, 4
    breq initKeypadClear               ; If all keys are scanned, repeat.
    sts PORTL, cmask        ; Otherwise, scan a column.
  
    ldi temp1, 0xFF         ; Slow down the scan operation.

delay:
    dec temp1
    brne delay              ; until temp1 is zero? - delay

    lds temp1, PINL          ; Read PORTL
    andi temp1, ROWMASK     ; Get the keypad output value
    cpi temp1, 0xF          ; Check if any row is low
    breq nextcol            ; if not - switch to next column

                            ; If yes, find which row is low
    ldi rmask, INITROWMASK  ; initialize for row check
    clr row

; and going into the row loop
rowloop:
    cpi row, 4              ; is row already 4?
    breq nextcol            ; the row scan is over - next column
    mov temp2, temp1
    and temp2, rmask        ; check un-masked bit
    breq convert            ; if bit is clear, the key is pressed
    inc row                 ; else move to the next row
    lsl rmask
    jmp rowloop
    
nextcol:                    ; if row scan is over
     lsl cmask
     inc col                ; increase col value
     jmp colloop            ; go to the next column
     
convert:
	cpi digit, 1			; button has not been released yet
	breq initKeypad			; don't use it, scan again

    cpi col, 3              ; If the pressed key is in col 3
    breq letters           ; we have letter
                            ; If the key is not in col 3 and
    cpi row, 3              ; if the key is in row 3,
    breq symbols            ; we have a symbol or 0
    mov temp1, row          ; otherwise we have a number 1-9
    lsl temp1
    add temp1, row
    add temp1, col          ; temp1 = row*3 + col
	subi temp1, -1
    jmp convert_end
    
letters:
	ldi digit, 1
	ldi debounceFlag, 1
    jmp initKeypad

symbols:
    cpi col, 0              ; Check if we have a star
    breq star
    cpi col, 1              ; or if we have zero
    breq zero
	ldi digit, 1
	ldi debounceFlag, 1		; # is pressed
    jmp initKeypad

star:
	ldi digit, 1
	ldi debounceFlag, 1
    jmp initKeypad

zero:
	ldi digit, 1
	ldi debounceFlag, 1
	jmp initKeypad			; no need for that

convert_end:
	ldi digit, 1
	ldi debounceFlag, 1					; disable keypad
	cpi waitingFlag, 0
	breq findItem 
    rjmp initKeypad         			; restart the main loop




findItem:
	sts NP, temp1						; Store the number been pressed
	ldi YH, high(QUANTITY)
	ldi YL, low(QUANTITY)

inventory:
	dec temp1
	cpi temp1, 0
	breq inStock
	adiw Y, 2
	rjmp inventory

outOfStock:
	lds temp, NP
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'O'
	do_lcd_data 'u'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'o'
	do_lcd_data 'f'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'c'
	do_lcd_data 'k'
	do_lcd_command 0b11000000	; break to the next line
	do_lcd_rdata temp
	rcall sleep_5ms

	ser temp					; let LED to be on as default
	out PORTC, temp

	clr r30					; clr button debounce counter
	clr r31
	clr counter

	ldi waitingFlag, 2		; enter led subroutine in TFOVR, DF=1
	rjmp initKeypad

inStock:
	ld temp, Y+				;quantity
	;mov temp1, temp
	ld temp2, Y				;price

	cpi temp, 0
	breq outOfStock

insertCoin:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'I'
	do_lcd_data 'n'
	do_lcd_data 's'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'c'
	do_lcd_data 'o'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 's'
	do_lcd_rdata temp			; count left
	;do_lcd_rdata temp			; coin Inserted
	do_lcd_command 0b11000000	; break to the next line
	do_lcd_rdata temp2			; coin left

	rjmp initKeypad



; main program ends here
;****************************************************************************************
;*******************************************    *****************************************
;*******************************************    *****************************************
;*******************************************    *****************************************
;***************************************   *    *   *************************************
;************************************  *   *    *   *   *********************************
;************************************  *   *    *   *  **********************************
;**************************************               ***********************************
;*****************************************          *************************************
; lcd stuff

lcd_command:
	out PORTF, lcd
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, lcd
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret


lcd_wait:
	push lcd
	clr lcd
	out DDRF, lcd
	out PORTF, lcd
	lcd_set LCD_RW

lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in lcd, PINF
	lcd_clr LCD_E
	sbrc lcd, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser lcd
	out DDRF, lcd
	pop lcd
	ret

; For LCD delay
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
		push r24
		push r25
		ldi r25, high(DELAY_1MS)
		ldi r24, low(DELAY_1MS)
delayloop_1ms:
		sbiw r25:r24, 1
		brne delayloop_1ms
		pop r25
		pop r24
		ret

sleep_5ms:
		rcall sleep_1ms
		rcall sleep_1ms
		rcall sleep_1ms
		rcall sleep_1ms
		rcall sleep_1ms
		ret
