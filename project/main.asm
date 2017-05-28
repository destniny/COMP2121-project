;
; project.asm
;
; Created: 20/05/2017 07:48:47
; Author : Edward & Gary 
;

.include "m2560def.inc"


.def temp = r16
.def waitingFlag = r17
.def counter = r19
.def lcd = r18
.def debounceFlag = r20
.def col = r21
.def row = r22
.def temp1 = r23
.def temp2 = r24
.def cmask = r25
.def rmask = r26
.def temp3 = r27
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
DC: .byte 2               ; Two-byte counter for counting seconds.   
TC:	.byte 2 
OC: .byte 2
RC: .byte 1
RCPATTERN: .byte 1
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
.org ADCCaddr
	jmp POT_Interrupt

defitem "8",  "4"  ;9  coin  quantity
defitem "6",  "3"  ;8
defitem "2",  "1"  ;7
defitem "3",  "8"  ;6
defitem	"0",  "2"  ;5
defitem "9",  "9"  ;4
defitem "1",  "4"  ;3
defitem "5",  "3"  ;2
defitem "6",  "2"  ;1

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
	clear TC
	clear DC
	clear OC
	;clear BC				; used to for buttons debounce
	;clear Quantity

	;initialize PB0 & PB1 button
	clr temp
	out DDRB, temp	
	out PORTB, temp			;Set pott D to input

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
;Time over flow

.equ START_SCREEN = 1
.equ OUT_OF_STOCK = 2
.equ CHANGE_LED = 3

clearReturning:
	clr debounceFlag
	clr temp
	ldi temp,(0<<PE4)	; Actually we are inputting data in PE4 (Confirmed from Email)
	out PORTE, temp
	pop temp
	rjmp Endif

initReturn:
	clr waitingFlag
	;out PORTC, debounceFlag

returnCoin:
	push temp
	lds temp, RC
	cpi temp, 0
	breq clearReturning
	cpi debounceFlag, 6
	breq returing
	dec temp
	sts RC, temp
	
	lds temp, RCPATTERN
	lsr temp
	sts RCPATTERN, temp
	out PORTC, temp
	ldi debounceFlag, 6
	ldi temp,(1<<PE4)	; Actually we are inputting data in PE4 (Confirmed from Email)
	out PORTE, temp
	pop temp
	rjmp newDebounce

returing:
	ldi debounceFlag, 4
	ldi temp,(0<<PE4)	; Actually we are inputting data in PE4 (Confirmed from Email)
	out PORTE, temp
	pop temp
	rjmp newDebounce

clearHash:
	clr waitingFlag

newDebounce:	
	;out PORTC, debounceFlag
	lds r24, DC
    lds r25, DC+1
    adiw r25:r24, 1			; Increase the temporary counter by one.

    cpi r24, low(2000)		; disable keypad for 50ms
    ldi temp, high(2000)	; DF=1
    cpc temp, r25
    brne notHundred			; 100 milliseconds have not passed
	clear DC
	cpi waitingFlag, 7
	breq initReturn
	cpi debounceFlag, 4
	breq returnCoin
	cpi debounceFlag, 6
	breq returnCoin
	clr debounceFlag		; renable keypad
    rjmp EndIF

notHundred: 		; Store the new value of the debounce counter.
	sts DC, r24
	sts DC+1, r25
	cpi waitingFlag, 7
	breq jmpEndif
	cpi debounceFlag, 4
	breq checkFlagSet
	cpi debounceFlag, 4
	breq checkFlagSet
	rjmp Endif


Timer0OVF:
	in temp, SREG
	push temp			; Prologue starts.
	push YH				; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24

checkReturn:
	cpi waitingFlag, 7
	breq newDebounce
	cpi waitingFlag, 6
	breq jmpChangeScreen
	cpi debounceFlag, 4					
	breq newDebounce
	cpi debounceFlag, 6					
	breq newDebounce

checkFlagSet:
	
	cpi waitingFlag, 5
	breq delivering
	cpi waitingFlag, START_SCREEN		; WF=1 starting screen
	breq starting 
	cpi waitingFlag, OUT_OF_STOCK		; out of stock screen: 1.turn the led on
	breq jmpTurnOnLED
	cpi waitingFlag, CHANGE_LED			; out of stock screen: 2.turn the led off and on
	breq jmpOutStock
	cpi waitingFlag, 4					; Waiting for Potentiometer
	breq checkHash
	cpi debounceFlag, 1					; WF=0 && DF=1: normal waiting but keypad pressed
	breq newDebounce


	rjmp Endif							; WF=0 && DF=0: noremal waiting but nothing pressed

delivering:
	ser temp
	out PORTC, temp
	ldi waitingFlag, CHANGE_LED
	rjmp outStock

jmpEndif:
	rjmp Endif

jmpTurnOnLED:
	jmp turnOnLED

jmpOutStock:
	jmp outStock

checkHash:
	cpi debounceFlag, 3
	breq jmpEndif

	adiw r31:r30, 1
	cpi r30, low(300)
	ldi temp, high(300)
	cpc r31, temp
	brne Not50ms
	; Potentiometer initialization
	ldi temp, (3<<REFS0 | 0<<ADLAR | 0<<MUX0)	;
	sts ADMUX, temp
	ldi temp, (1<<MUX5)	;
	sts ADCSRB, temp
	ldi temp, (1<<ADEN | 1<<ADSC | 1<<ADIE | 5<<ADPS0)	; Prescaling
	sts ADCSRA, temp
	clr r30
	clr r31
	rjmp Endif

Not50ms:
	rjmp Endif

jmpChangeScreen:
	jmp changeScreen
	
starting:
	cpi debounceFlag, 1		; WF=1 && DF=1: starting screen can be interrupt by 
	breq jmpChangeScreen	; pressing keypad
	lds r24, TC         ;load TC
	lds r25, TC+1 
	adiw r25:r24, 1
	cpi r24, low(7812)
	ldi temp, high(7812)
	cpc r25, temp
	brne NotaSecond
	clear TC
	cpi waitingFlag, START_SCREEN		; recheck if it's still in starting screen
	breq waiting

	rjmp Endif



NotaSecond:
	sts TC, r24
	sts TC+1, r25
	rjmp Endif



waiting:
	inc counter
	;out PORTC, counter
	cpi counter, 3
	breq isThree
	rjmp Endif
	


isThree:
	clr waitingFlag
	clr debounceFlag
	clr counter
	in temp, PORTE
	ldi temp, (0 << PE4)
	out PORTE, temp
	;out PORTC, counter
	rjmp changeScreen



turnOnLED:
	ser temp
	out PORTC, temp
	ldi waitingFlag, CHANGE_LED

/*button:						; debounce for buttons 
	lds r24, BC					; no needed but just in case
	lds r25, BC+1 
	adiw r25:r24, 1
	cpi r24, low(500)
	ldi temp, high(500)
	cpc r25, temp
	brne Nota500*/


	;ldi debounceFlag, 2			; enable bottuns
	
	;clear BC
	;rjmp outStock

/*Nota500:
	sts BC, r24
	sts BC+1, r25*/

outStock:
	lds r24, OC         ;load OC
	lds r25, OC+1 
	adiw r25:r24, 1
	cpi r24, low(3906)
	ldi temp, high(3906)
	cpc r25, temp
	brne NotaHalfSecond
	clear OC
	rjmp LED



LED:
	cpi counter, 5
	breq isThree
	inc counter
	mov temp, counter
	andi temp, ODDEVENMASK
	cpi temp, 0			;even
	breq turnOnLED		
	clr temp
	out PORTC, temp
	rjmp Endif

NotaHalfSecond:
	sts OC, r24
	sts OC+1, r25

Endif:
	pop	r24
	pop	r25
	pop	YL
	pop	YH
	pop	temp
	out SREG, temp
	reti

PB0_Interrupt:
	cpi debounceFlag, 2				;if the buttons are still debouncing
	brne return						;Do nothing
	push temp
	in temp, SREG
	push temp
	clear OC
	clr temp
	out PORTC, temp
	pop temp
	out SREG, temp
	pop temp
	rjmp ChangeScreen

PB1_Interrupt:
	cpi debounceFlag, 2
	brne return
	push temp
	in temp, SREG
	push temp
	clear OC
	clr temp
	out PORTC, temp
	pop temp
	out SREG, temp
	pop temp
	rjmp changeScreen

return:
	reti

POT_Interrupt:
	cpi debounceFlag, 3
	breq continue
	push temp
	in temp, SREG
	push temp
	push r25
	push r24
	lds r24, ADCL
	lds r25, ADCH
	;out PORTC, r25		; for debug
	cpi r24, 0
	ldi temp, 0
    cpc r25, temp
	breq setPOTMinFlag
	cpi r24, 0xFF		; ADCL/H  is 10 bits reg
	ldi temp, 3
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
	cpi temp3, 0
	brne continue
	inc counter
	ldi temp3, 1
	;do_lcd_rdata debounceFlag
	cpi counter, 2
	brne continue
	ldi debounceFlag, 3
	;ldi temp, (0<<ADEN | 1<<ADSC | 0<<ADIE)	; Prescaling
	;sts ADCSRA, temp
	rjmp continue

setPOTMaxFlag:
	cpi temp3, 1
	brne continue
	clr temp3
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
	;clr temp
	;out PORTC, temp
	;push temp					; Display current Remaining coin
	;lds temp, RC
	;out PORTC, temp
	;pop temp

	cpi debounceFlag, 1			; 1.skiped starting screen
	breq keepDebounce			; disable keypad input for more 50 ms 
	cpi debounceFlag, 2			; 1.skiped outOfStock screen
	breq returnClear			; disable button & back to normal

	clr counter
	;clr debounceFlag			; # pressed
	ldi waitingFlag, 7
	rjmp Endif


jmpInitReturn:
	rjmp initReturn



returnClear:
	clear OC
	clear DC
	clear TC
	clr counter
	clr debounceFlag
	clr waitingFlag
	reti

keepDebounce:					; disable keypad input for more 50 ms
	clr waitingFlag				; Back to Normal
	clr counter
	clear OC
	clear TC
	clear DC
	rjmp newDebounce			; debounceFlag will be reset in 50ms

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
	ldi waitingFlag, START_SCREEN	;initialize the waiting from starting screen
	
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
	clr counter

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
;initKeypadClear:
	;clr digit
initKeypad:
    ldi cmask, INITCOLMASK  ; initial column mask
    clr col                 ; initial column
	clr temp
	clr temp1
	clr temp2
	; debounce check
	cpi debounceFlag, 1		; if the button is still debouncing, ignore the keypad
	breq initKeypad	
	cpi debounceFlag, 2		; if the button is still debouncing, ignore the keypad
	breq initKeypad	
	cpi debounceFlag, 3		; if the one coin has been inserted
	breq goPOT	

	cpi waitingFlag, OUT_OF_STOCK
	breq initKeypad
	cpi waitingFlag, CHANGE_LED
	breq initKeypad
	cpi waitingFlag, 6
	breq initKeypad
	cpi waitingFlag, 7
	breq initKeypad

colloop:
    cpi col, 4
    breq initKeypad               ; If all keys are scanned, repeat.
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

goPOT:
	ldi debounceFlag, 3
	rjmp POT
    
nextcol:                    ; if row scan is over
     lsl cmask
     inc col                ; increase col value
     jmp colloop            ; go to the next column
     
convert:
	; NOTE: cols and rows are counter-intuitive (flipped)
	;mov temp, col
	;mov col, row
	;mov row, temp

	;out PORTC, col

    cpi col, 3              ; If the pressed key is in col 3
    breq letters           ; we have letter
    ;breq convert_end
                            ; If the key is not in col 3 and
    cpi row, 3              ; if the key is in row 3,
    breq symbols            ; we have a symbol or 0
	;breq convert_end

    ;mov temp1, row          ; otherwise we have a number 1-9
    ;lsl temp1
	;add col, 1
	;add row, 1
    ;add temp1, col
    ;add temp1, row          ; temp1 = 1 + row + col
    ;subi temp1, -'1'        ; add the value of character '1'
    mov temp1, row          ; otherwise we have a number 1-9
    lsl temp1
    add temp1, row
    add temp1, col          ; temp1 = row*3 + col
	subi temp1, -1
    jmp convert_end
    
letters:
    ;ldi temp1, 'A'
    ;add temp1, row          ; Get the ASCII value for the key
	;clr temp1
	ldi debounceFlag, 1
    jmp initKeypad

symbols:
    cpi col, 0              ; Check if we have a star
    breq star
    cpi col, 1              ; or if we have zero
    breq zero
    ;ldi temp1, '#'         ; if not we have hash
	;clr temp1				; TEMP: not handling the hash now
	cpi waitingFlag, 4
	breq abort
	ldi debounceFlag, 1		; # is pressed
    jmp initKeypad

abort:
	ldi debounceFlag, 4
	pop YH
	pop YL
	pop counter
	pop temp3
	pop temp2
	pop temp1
	;out PORTC, temp1
	sts RC, temp1
	sts RCPATTERN, counter
	clr counter
	ldi waitingFlag, 6
	rjmp initKeypad

star:
    ;ldi temp1, '*'          ; set to star
	;clr temp1
	ldi debounceFlag, 1
    jmp initKeypad
zero:
    ;ldi temp1, 0          ; set to zero in binary
	ldi debounceFlag, 1
	jmp initKeypad			; no need for that

convert_end:
	;do_lcd_rdata temp1		; Key has been pressed value in TEMP1
	cpi waitingFlag, 4		; if it's for potentio
	breq goInitial			; keep disable keypad in starting screen
	ldi debounceFlag, 1		; disable keypad
	cpi waitingFlag, START_SCREEN		; if it's for starting screen just skip it
	breq goInitial			; keep disable keypad in starting screen
	push temp1
	;subi temp1,48
	;clr counter
   ; rjmp initKeypad         	; restart the main loop


findItem:
	clr debounceFlag
	ldi YH, high(QUANTITY)
	ldi YL, low(QUANTITY)
	;ldi temp, 6
	;compare if it is in inventory
	;if is rjmp insertcoin
	;if not rjmp outOfStock


inventory:
	dec temp1
	cpi temp1, 0
	breq inStock
	adiw Y, 2
	rjmp inventory

goInitial:
	jmp initKeypad

outOfStock:
	pop temp1
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

	do_lcd_rdata temp1
	rcall sleep_5ms
	clr counter
	ldi debounceFlag, 2		; keypad keep disable
	ldi waitingFlag, OUT_OF_STOCK		; enter led subroutine in TFOVR
	rjmp initKeypad
	
inStock:
	ld temp,Y+				;quantity
	mov temp3, temp
	ld temp2, Y				;price
	
	cpi temp, 0
	breq outOfStock
	clr temp1

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
	do_lcd_rdata temp3
	do_lcd_rdata temp1

	do_lcd_command 0b11000000	; break to the next line
	do_lcd_rdata temp2
	push temp1
	push temp2
	push temp3
	push counter
	push YL
	push YH
	clr counter
	clr temp2
	clr temp3
	

initPOT:								; WF=0 DF=1 
	ldi waitingFlag, 4					; WF=4 DF=3 diable keyPad but "#" in normal mode
										; waiting for twisted

	clr debounceFlag							; for the num of coins inserted

POT:
	cpi debounceFlag, 3					; see if the twist has been twisted 
	brne gogoInitial
	ldi temp, (0<<ADEN | 1<<ADSC | 0<<ADIE)	; Prescaling
	sts ADCSRA, temp
	clr debounceFlag
	pop YH
	pop YL
	pop counter
	pop temp3
	pop temp2
	pop temp1
	inc temp1	

	lsl counter
	inc counter
	out PORTC, counter
	
	subi temp2, 1						; 
	cpi temp2, 0						; if all coin has been inserted
	brne goInsert						; refresh the screen
	clr waitingFlag
	rjmp delivery

goInsert:
	rjmp insertCoin

gogoInitial:
	jmp initKeypad

	
delivery:
	subi temp3, 1
	st -Y, temp3
	clr counter
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

	ldi temp,(1<<PE4)	; Actually we are inputting data in PE4 (Confirmed from Email)
	out DDRE, temp
	out PORTE, temp
	ldi waitingFlag, 5
	ldi debounceFlag, 1

	rjmp gogoInitial

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
