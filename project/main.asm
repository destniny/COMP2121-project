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
.def outFlag = r27
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
	
	.db @0, @1, @2, 0
	.set T = PC
	
.endmacro

.dseg
DC: .byte 2               ; Two-byte counter for counting seconds.   
TC:	.byte 2 
OC: .byte 2

.cseg


.org 0x0000
	jmp RESET

defitem "1",  "8",  "4"
defitem "2",  "6",  "3"
defitem "3",  "2",  "1"
defitem "4",  "3",  "8"
defitem	"5",  "0",  "2"
defitem "6",  "9",  "9"
defitem "7",  "1",  "4"
defitem "8",  "5",  "3"
defitem "9",  "6",  "2"

.org OVF0addr
	jmp Timer0OVF ; Jump to the interrupt handler for
					; Timer0 overflow
	jmp DEFAULT ; default service for all other interrupts.

DEFAULT: 
	reti ; no service continued


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


RESET:
	ldi YL, low(RAMEND)
	ldi YH, high(RAMEND)
	out SPH, YH
	out SPL, YL				;reset SP

	;initialize Z
	ldi ZH, high(T << 1)
	ldi ZL, low(T << 1)
	

	;initilize LED
    ser temp
	out DDRC, temp			;Set port C to output
	clr temp
	out PORTC, temp			;pass the lower pattern to portC
	
	;initialize timer counter
	clear TC
	clear DC
	clear OC

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


Timer0OVF:
	in temp, SREG
	push temp			; Prologue starts.
	push YH				; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24

checkFlagSet:				; if either flag is set - run the debounce timer
	
	cpi waitingFlag, 1
	breq common 
	cpi waitingFlag, 2
	breq turnOnLED
	cpi waitingFlag, 3
	breq outStock
	cpi debounceFlag, 1
	breq newDebounce
	

common:
	cpi debounceFlag, 1
	breq jmpChangeScreen
	lds r24, TC         ;load TC
	lds r25, TC+1 
	adiw r25:r24, 1
	cpi r24, low(7812)
	ldi temp, high(7812)
	cpc r25, temp
	brne NotaSecond
	clear TC
	cpi waitingFlag, 1
	breq waiting

	rjmp Endif

turnOnLED:
	ser temp
	out PORTC, temp
	ldi waitingFlag, 3

outStock:
	lds r24, OC         ;load TC
	lds r25, OC+1 
	adiw r25:r24, 1
	cpi r24, low(3906)
	ldi temp, high(3906)
	cpc r25, temp
	brne NotaHalfSecond
	clear OC
	cpi waitingFlag, 3
	breq LED

	rjmp Endif

jmpChangeScreen:
	jmp changeScreen

LED:
	cpi counter, 5
	breq isThree
	inc counter
	mov temp, counter
	andi temp, ODDEVENMASK
	cpi temp, 0
	breq turnOnLED
	clr temp
	out PORTC, temp
	rjmp Endif



NotaSecond:
	sts TC, r24
	sts TC+1, r25
	rjmp Endif
	
newDebounce:	
	lds r24, DC
    lds r25, DC+1
    adiw r25:r24, 1 ; Increase the temporary counter by one.

    cpi r24, low(1000)		; Check if (r25:r24) = 390 ; 7812 = 10^6/128/20 ; 50 milliseconds
    ldi temp, high(1000)		;390 = 10^6/128/20 
    cpc temp, r25
    brne notHundred			; 100 milliseconds have not passed
	clear DC
	clr debounceFlag
			;	once 100 milliseconds have passed, set the debounceFlag to 0	; Reset the debounce counter.
    rjmp EndIF

waiting:
	cpi counter, 3
	breq isThree
	inc counter
	out PORTC, counter
	rjmp Endif

isThree:
	clr waitingFlag
	clr counter
	out PORTC, counter
	rjmp jmpChangeScreen



notHundred: 		; Store the new value of the debounce counter.
	sts DC, r24
	sts DC+1, r25
	rjmp Endif

NotaHalfSecond:
	sts OC, r24
	sts OC+1, r25
	rjmp Endif



Endif:
	pop	r24
	pop	r25
	pop	YL
	pop	YH
	pop	temp
	out SREG, temp
	reti

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
	clr debounceFlag
	clr waitingFlag
	rcall sleep_5ms
	rjmp Endif


main:
	;set timer interrupt
	clr counter
	clr debounceFlag
	ldi waitingFlag, 1
	clr temp
	out TCCR0A, temp
	ldi temp, (1<<CS01)
	out TCCR0B, temp		; Prescaling value=8
	ldi temp, 1<<TOIE0		; Enable timeroverflow flag
	sts TIMSK0, temp
	sei						; Enable global interrupt

;initKeypadClear:
	;clr digit
initKeypad:
    ldi cmask, INITCOLMASK  ; initial column mask
    clr col                 ; initial column
	clr temp
	clr temp1
	clr temp2
	cpi waitingFlag, 2
	breq initKeypad
	cpi waitingFlag, 3
	breq initKeypad

	; debounce check
	cpi debounceFlag, 1		; if the button is still debouncing, ignore the keypad
	breq initKeypad	

	;ldi debounceFlag, 1		; otherwise set the flag now to init the debounce

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
    jmp initKeypad

symbols:
    cpi col, 0              ; Check if we have a star
    breq star
    cpi col, 1              ; or if we have zero
    breq zero
    ;ldi temp1, '#'         ; if not we have hash
	;clr temp1				; TEMP: not handling the hash now
    jmp initKeypad
star:
    ;ldi temp1, '*'          ; set to star
	;clr temp1
    jmp initKeypad
zero:
    ldi temp1, 0          ; set to zero in binary
	jmp convert_end

goInitial:
	jmp initKeypad

convert_end:
	;out PORTC, temp1
	do_lcd_rdata temp1	; output current number
	ldi debounceFlag, 1
	cpi waitingFlag, 1
	breq goInitial

   ; rjmp initKeypad         	; restart the main loop


inInventory:
	;compare if it is in inventory
	;if is rjmp insertcoin
	;if not rjmp outOfStock

outOfStock:

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
	ldi waitingFlag, 2
	rjmp initKeypad

