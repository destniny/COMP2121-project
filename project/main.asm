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
TC: .byte 2						; Thousand Counter
TF: .byte 1						; Twist Flag
RC: .byte 1						; Return Coin
NP: .byte 1						; Number Pressed
RF: .byte 1
RCPATTERN: .byte 1
PR: .byte 1						; Price
QN: .byte 1						; Qantity
QUANTITY: .byte 18
BE: .byte 1
BF: .byte 1

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
.org ADCCaddr
	jmp POT_Interrupt

defitem 1,  5  ;9  coin  quantity
defitem 2,  3  ;8
defitem 1,  0  ;7
defitem 2,  255  ;6
defitem	1,  2  ;5
defitem 2,  9  ;4
defitem 1,  4  ;3
defitem 2,  3  ;2
defitem 1,  0  ;1


RESET:
	ldi YL, low(RAMEND)
	ldi YH, high(RAMEND)
	out SPH, YH
	out SPL, YL				;reset SP
	
	;initilize LED
    ser temp
	out DDRC, temp			;Set port C to output
	out DDRG, temp
	clr temp
	out PORTC, temp
	out PORTG, temp

	;initialize timer counter
	clear DC
	clear OC
	clear HC
	clear TC

	;initialize motor
	ldi temp,(1<<PE4)
	out DDRE, temp

	;initialize speaker
	clr temp
	sts BE, temp
	;ldi temp, (1<<PB0)
	ser temp
	out DDRB, temp
	;out PORTB, temp			;Set pott B to input

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

returning:
	lds temp, RF
	cpi temp, 0		; if hasn't reached max, ignore the twist
	brne clearRF

	inc temp
	sts RF, temp
	ldi temp,(1<<PE4)			; start the motor
	out PORTE, temp


	lds temp, RCPATTERN
	lsr temp
	out PORTC, temp
	sts RCPATTERN, temp

	rjmp checkFlagSet

clearRF:
	clr temp
	sts RF, temp

	lds temp, RC
	dec temp
	sts RC, temp

	ldi temp,(0<<PE4)			; stop the motor
	out PORTE, temp
	rjmp checkFlagSet

notThousand: 		; Store the new value of the debounce counter.
	sts TC, r24
	sts TC+1, r25
	rjmp checkFlagSet

returnCoin:
	cpi debounceFlag, 4
	breq jmpChangeScreen
	lds r24, TC
    lds r25, TC+1
    adiw r25:r24, 1			; Increase the temporary counter by one.

    cpi r24, low(1953)		; disable keypad for 0.25s
    ldi temp, high(1953)	; DF=1
    cpc temp, r25
    brne notThousand	; 100 milliseconds have not passed
	clear TC
	rjmp returning



jmpReturnCoin:
	rjmp returnCoin


jmpKeyDebounce:
	rjmp keyDebounce

jmpCheckHash:
	rjmp checkHash

jmpHalfSecond:
	rjmp halfSecond

jmpCheckBeep:
	rjmp checkBeep

Timer0OVF:
	push temp
	in temp, SREG
	push temp			; Prologue starts.
	push YH				; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	
checkReturn:
	lds temp, RC
	cpi temp, 0
	brne jmpreturnCoin

checkFlagSet:
	cpi waitingFlag, 1		; WF=1 starting screen
	breq starting
	cpi waitingFlag, 2		; out of stock screen: 1.turn the led on
	breq jmpCheckBeep
	cpi waitingFlag, 3		; inserting screen
	breq jmpCheckHash			; waiting for potentiometer input and keep checking if there is a hash pressed
	cpi waitingFlag, 4		; delivering screen
	breq jmpCheckBeep	
	cpi waitingFlag, 5		; admin screen
	breq admin		
	cpi debounceFlag, 1					; DF=1: normal waiting but keypad pressed
	breq jmpKeyDebounce
	cpi debounceFlag, 5					; DF=1: normal waiting but keypad pressed
	breq oneSecond

	clear DC
	clear OC
	clr counter

	rjmp Endif

jmpChangeScreen:
	jmp changeScreen

admin:
	lds temp, BF
	cpi temp, 1
	breq oneSecond
adminContinue:
	cpi debounceFlag, 4					; DF=1: normal waiting but keypad pressed
	breq jmpChangeScreen
	cpi debounceFlag, 1					; DF=1: normal waiting but keypad pressed
	breq jmpKeyDebounce
	adiw r31:r30,1			; Everytime i increment DebounceCounter
	cpi r30,low(1000)		; Check if the debounceCounter reaches ~80ms, we enables the flag
	ldi temp,high(1000)
	cpc r31,temp
	brne jmpEndif
	ldi DebounceFlag, 2		;enable button to interrpt the program
	clr r30
	clr r31
	clear DC
jmpEndif:
	rjmp Endif


;////////////////////////////////////////////////////////////
starting:
	cpi digit, 1			; WF=1 starting screen can be interrupt by 
	breq jmpChangeScreen	; pressing keypad
oneSecond:
	lds r24, OC
	lds r25, OC+1 
	adiw r25:r24, 1
	cpi r24, low(7812)
	ldi temp, high(7812)
	cpc r25, temp
	brne NotaSecond
	clear OC
	cpi waitingFlag, 5
	breq clearBF
	cpi debounceFlag, 5					; DF=5: * holding wanna enter admin mode
	breq countFive

countThree:
	inc counter
	cpi counter, 3
	breq isThree
	rjmp Endif

adminBeep:
	rcall beep
	rjmp adminContinue

clearBF:
	clr temp
	sts BF, temp
	rjmp adminContinue

countFive:
	clr debounceFlag
	inc counter
	cpi counter, 5
	breq isFive
	rjmp Endif

isFive:
	ldi waitingFlag, 6				; triggering to Admin mode
	ldi temp, 1
	sts BF, temp
	clear OC
	clr counter
	rjmp Endif

NotaSecond:
	sts OC, r24
	sts OC+1, r25
	cpi waitingFlag, 5
	breq adminBeep
	clr debounceFlag
	rjmp Endif

isThree:
	;ldi debounceFlag, 1				;incase button pressed
	rjmp changeScreen

;//////////////////////////////////////////////////////////////////////
;######################################################################
keyDebounce:
	;out PORTC, debounceFlag
	rcall beep
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


;#####################################################################################
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

turnOnLED:
	ser temp
	out PORTC, temp
	out PORTG, temp
	cpi waitingFlag, 4
	breq halfSecond

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
	out PORTG, temp
	rjmp Endif

NotaHalfSecond:
	sts HC, r24
	sts HC+1, r25
	rjmp Endif

doBeep:
	rcall beep
	rjmp finishBeep

checkBeep:
	cpi counter, 2
	brlt doBeep

finishBeep:
	cpi waitingFlag, 2
	breq jmpButtonDebounce
	cpi waitingFlag, 4
	breq halfSecond
	rjmp Endif


jmpButtonDebounce:
	rjmp buttonDebounce

goKeyDebounce:
	rjmp keyDebounce

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
checkHash:
	cpi debounceFlag, 4
	breq goChangeScreen
	cpi debounceFlag, 1
	breq gokeyDebounce
	cpi debounceFlag, 3					; twisted, waiting for main to clr debounce
	breq Endif
	clr debounceFlag
	adiw r31:r30, 1
	cpi r30, low(300)
	ldi temp, high(300)
	cpc r31, temp
	brne Not50ms
	ldi temp, (1<<ADEN | 1<<ADSC | 1<<ADIE | 5<<ADPS0)	; Read the potentiometer
	sts ADCSRA, temp
	clr r30
	clr r31
	rjmp Endif

Not50ms:
	rjmp Endif

goChangeScreen:
	rjmp changeScreen

Endif:
	pop	r24
	pop	r25
	pop	YL
	pop	YH
	pop	temp
	out SREG, temp
	pop temp
	reti

returni:
	pop temp
	out SREG, temp
	pop temp
	reti

increaseInv:
	push temp
	in temp, SREG
	push temp
	lds temp, QN
	cpi temp, 255
	breq returni
	push YL
	push YH
	inc temp
	sbiw Y, 1
	st Y, temp
	ldi debounceFlag, 8
	clr r30
	clr r31
	pop YH
	pop YL
	pop temp
	out SREG, temp
	pop temp
	reti

decreaseInv:
	push temp
	in temp, SREG
	push temp
	lds temp, QN
	cpi temp, 0
	breq returni
	push YL
	push YH
	dec temp
	sbiw Y, 1
	st Y, temp
	ldi debounceFlag, 8
	clr r30
	clr r31
	pop YH
	pop YL
	pop temp
	out SREG, temp
	pop temp
	reti

PB0_Interrupt:
	cpi debounceFlag, 2				;if the buttons are still debouncing
	brne return						;Do nothing
	cpi waitingFlag, 5
	breq increaseInv
	push temp
	in temp, SREG
	push temp
	clr r30
	clr r31
	clr temp
	out PORTC, temp
	out PORTG, temp
	pop temp
	out SREG, temp
	pop temp
	rjmp changeScreen

PB1_Interrupt:
	cpi debounceFlag, 2
	brne return
	cpi waitingFlag, 5
	breq decreaseInv
	push temp
	in temp, SREG
	push temp
	clr r30
	clr r31
	clr temp
	out PORTC, temp
	out PORTG, temp
	pop temp
	out SREG, temp
	pop temp
	rjmp changeScreen

return:
	reti

POT_Interrupt:
	cpi debounceFlag, 3
	breq return
	push temp
	in temp, SREG
	push temp
	push r25
	push r24
	lds r24, ADCL
	lds r25, ADCH
	;out PORTC, r24		; for debug
	cpi r24, 0
	ldi temp, 0
    cpc r25, temp
	breq setPOTMinFlag
	cpi r24, 0xFF		; ADCL/H  is 10 bits reg
	ldi temp, 0b11
    cpc r25, temp
	breq setPOTMaxFlag

continue:
	pop r24
	pop r25
	pop temp
	out SREG, temp
	pop temp
	reti

setPOTMinFlag:
	lds temp, TF
	cpi temp, 0		; if hasn't reached max, ignore the twist
	brne continue
	inc temp
	sts TF, temp
	inc counter
	;out PORTC, counter
	cpi counter, 2
	brne continue
	ldi debounceFlag, 3
	rjmp continue

setPOTMaxFlag:
	lds temp, TF
	cpi temp, 1
	brne continue
	clr temp
	sts TF, temp
	;do_lcd_rdata debounceFlag
	rjmp continue

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

	clr temp
	out PORTC, temp
	out PORTG, temp

	ldi temp,(0<<PE4)			; stop the motor
	out PORTE, temp

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
	cpi debounceFlag, 4			; button interrupted
	breq retCoin			; clear debounceFlag
	clr debounceFlag
	rjmp Endif
retCoin:
	ldi debounceFlag, 1
	clr temp
	sts RF, temp
	rjmp keyDebounce

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

	; Potentiometer initialization
	ldi temp, (3<<REFS0 | 0<<ADLAR | 0<<MUX0)	;
	sts ADMUX, temp
	ldi temp, (1<<MUX5)	;
	sts ADCSRB, temp

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
	;subi temp, 48
	sbiw Z, 1
	inc counter
	st Y+,temp
	cpi counter, 18
	brne initQuantity
	clr r30
	clr r31
	pop counter

	ldi waitingFlag, 1	;initialize the waiting from starting screen

initKeypadClear:
	clr digit
initKeypad:
	;out PORTC, counter
	; waitingFlag check
	cpi waitingFlag, 2		; WF=2 DF=1 out of stock screen
	breq initKeypad
	cpi waitingFlag, 6		; WF=0 DF=6 enter Admin mode
	breq goInitAdmin
	; debounce check
	cpi debounceFlag, 1		; WF=0 DF=1 key pressed
	breq initKeypad
	cpi debounceFlag, 3		; WF=3 DF=3 insertion finished
	breq goPOT
	
	cpi debounceFlag, 8		; WF=5 DF=8 refresh admin screen
	breq goAdmin

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

goPOT:
	rjmp POT
goInitAdmin:
	rjmp initAdmin
goAdmin:
	rjmp adminMode
    
letters:
	ldi digit, 1
	cpi waitingFlag, 5
	breq letterAdmin
	ldi debounceFlag, 1
    jmp initKeypad

letterAdmin:
	cpi row, 0
	breq A
	cpi row, 1
	breq B
	cpi row, 2
	breq C

nothing:
	ldi debounceFlag, 1
    jmp initKeypad

A:
	ldi debounceFlag, 1
	lds temp2, PR
	cpi temp2, 3
	breq nothing
	inc temp2
	st Y, temp2
	rjmp adminMode

B:
	ldi debounceFlag, 1
	lds temp2, PR
	cpi temp2, 0
	breq nothing
	dec temp2
	st Y, temp2
	rjmp adminMode
C:
	ldi debounceFlag, 1
	push temp
	push YL
	push YH
	sbiw Y, 1
	clr temp
	st Y, temp
	pop YH
	pop YL
	pop temp
	rjmp adminMode

symbols:
    cpi col, 0              ; Check if we have a star
    breq star
    cpi col, 1              ; or if we have zero
    breq zero
	ldi digit, 1
	ldi debounceFlag, 1
	cpi waitingFlag, 3		; # is pressed inserting screen
	breq abort
	cpi waitingFlag, 5		; admin screen
	breq abortAdmin
	
    jmp initKeypad

abort:
	pop YL
	pop YH
	pop counter							; coin inserted
	pop temp2							; coin left
	pop temp1
	pop temp							; count left
	;out PORTC, counter
	sts RC, counter
	sts RCPATTERN, temp1
	clr counter
	ldi debounceFlag, 4					; # pressed when it's inserting WF=3
	jmp initKeypad

abortAdmin:
	ldi debounceFlag, 4			; #is pressed when it's in admin mode WF=5
	rjmp initKeypad

star:
	cpi waitingFlag, 0
	breq goingAdmin
	ldi digit, 1
	ldi debounceFlag, 1
    jmp initKeypad

goingAdmin:
	ldi debounceFlag, 5				; * has been pressed
	jmp initKeypad

zero:
	ldi digit, 1
	ldi debounceFlag, 1
	jmp initKeypad			; no need for that

convert_end:
	sts NP, temp1
	ldi digit, 1
	ldi debounceFlag, 1					; disable keypad
	cpi waitingFlag, 5
	breq adminMode
	
	cpi waitingFlag, 0
	breq findItem
	
    rjmp initKeypad         			; restart the main loop

initAdmin:
	ldi waitingFlag, 5
	clr r30								; for button debounce
	clr r31
	ldi temp1, 1
	sts NP, temp1
adminMode:
	lds temp1, NP
	do_lcd_command 0b00000001 ; clear display

	do_lcd_data 'A'
	do_lcd_data 'd'
	do_lcd_data 'm'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data ' '
	do_lcd_data 'm'
	do_lcd_data 'o'
	do_lcd_data 'd'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_rdata temp1
	
	sts NP, temp1
	rjmp findItem

findItem:
	sts NP, temp1						; Store the number been pressed
	ldi YH, high(QUANTITY)
	ldi YL, low(QUANTITY)

inventory:
	dec temp1
	cpi temp1, 0
	breq goInStock
	adiw Y, 2
	rjmp inventory

goInStock:
	rjmp inStock

showAdmin:
	do_lcd_command 0b11000000	; break to the next line
	lds temp, QN
	rcall convert_digits
	;do_lcd_rdata temp
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data '$'
	lds temp2, PR
	do_lcd_rdata temp2
	;ldi debounceFlag, 9
	;out PORTC, debounceFlag
	rjmp initKeypad

goOutOfStock:
	rjmp outOfStock

makePattern:
	push temp
	push temp1
	push temp2
	clr temp1
	clr temp2
loopPattern:
	cpi temp, 0
	breq showPattern
	cpi temp1, 0xFF
	breq overEight
	lsl temp1
	inc temp1
	dec temp
	
	rjmp loopPattern

overEight:
	cpi temp2, 0b11
	brne notTen
	clr temp1
	clr temp2
	rjmp loopPattern
notTen:
	lsl temp2
	inc temp2
	dec temp
	rjmp loopPattern
showPattern:
	out PORTC, temp1
	out PORTG, temp2
	pop temp2
	pop temp1
	pop temp
	rjmp showAdmin

inStock:
	ld temp, Y+				;quantity
	sts QN, temp
	ld temp2, Y				;price
	sts PR, temp2

	cpi waitingFlag, 5
	breq makePattern

	cpi temp, 0
	breq goOutOfStock
	clr counter

insertCoin:
	push temp
	push temp1
	push temp2
	push counter
	push YH
	push YL
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
	rcall convert_digits
	;do_lcd_rdata temp			; count left
	;do_lcd_rdata temp			; coin Inserted
	do_lcd_command 0b11000000	; break to the next line
	do_lcd_rdata temp2			; coin left

initPOT:								; WF=0 DF=1 
	ldi waitingFlag, 3					; WF=3 DF=0 diable keyPad but "#" in normal mode
										; waiting for twisted

	clr temp							; Flag for two side
	sts TF, temp
	clr counter
	;clr debounceFlag							; for the num of coins inserted
	clr r30
	clr r31

POT:
	cpi debounceFlag, 3					; see if the twist has been twisted 
	brne goInitial
	ldi temp, (0<<ADEN | 1<<ADSC | 0<<ADIE)	; disable potentiometer
	sts ADCSRA, temp
	clr debounceFlag
	pop YL
	pop YH
	pop counter
	pop temp2							; coin left
	pop temp1
	pop temp							; count left
	inc counter

	lsl temp1
	inc temp1
	out PORTC, temp1
	
	subi temp2, 1						; 
	cpi temp2, 0						; if all coin has been inserted
	brne goInsert						; refresh the screen
	clr waitingFlag
	rjmp delivery

goInsert:
	rjmp insertCoin
goInitial:
	rjmp initKeypad

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
	out PORTG, temp

	clr r30					; clr button debounce counter
	clr r31
	clr counter

	ldi waitingFlag, 2		; enter led subroutine in TFOVR, DF=1
	rjmp initKeypad

delivery:
	subi temp, 1
	st -Y, temp
	do_lcd_command 0b00000001 ; clear display

	do_lcd_data 'D'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'i'
	do_lcd_data 'v'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'

	do_lcd_command 0b11000000	; break to the next line

	ser temp					; let LED to be on as default
	out PORTC, temp
	out PORTG, temp

	ldi temp,(1<<PE4)			; start the motor
	out PORTE, temp

	ldi waitingFlag, 4

	clr counter
	rjmp goInitial





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

convert_digits:
	push digit
	push temp
	clr digit
	;push temp
	push temp1
	clr temp1
	;push temp2
checkHundreds:
	cpi temp, 100			; is the number still > 100?
	brsh hundredsDigit		; if YES - increase hundreds digit
	cpi digit, 0		
	brne showHundredsDigit	; If digit ! 0 => this digit goes into stack
		
checkTensInit:
	clr digit
checkTens:
	cpi temp, 10			; is the number still > 10? 
	brsh tensDigit			; if YES - increase tens digit
	cpi temp1, 0
	brne showTensDigit
	cpi digit, 0			; is tens digit = 0?
	brne showTensDigit		; if digit != 0 push it to the stack	
	
checkOnes:
	clr digit
	mov digit, temp			; whatever is left in temp is the ones digit
	; now all digit temp data is in the stack
	; unload data into temp2, temp1, temp
	; and the do_lcd_rdata in reverse order
	; this will display the currentNumber value to LCD
	; it's not an elegant solution but will do for now
	do_lcd_rdata digit
	pop temp1
	pop temp
	pop digit
	ret

; hundreds digit
hundredsDigit:
	ldi temp1, 1
	inc digit
	subi temp, 100			; and subtract a 100 from the number
	rjmp checkHundreds		; check hundreds again

; tens digit
tensDigit:
	inc digit				; if YES increase the digit count
	subi temp, 10			; and subtract a 10 from the number
	rjmp checkTens			; check tens again

showHundredsDigit:
	do_lcd_rdata digit
	rjmp checkTensInit

showTensDigit:
	do_lcd_rdata digit
	rjmp checkOnes


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
beep:
	lds temp, BE
	com temp
	out PORTB, temp
	sts BE, temp
	ret