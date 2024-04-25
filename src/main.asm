/*
 * ATTINY4313_matrix_asm.asm
 *
 *  Created: 2019-03-25 22:06:47
 *   Author: Mateusz
 */ 

.NOLIST
		.INCLUDE "tn4313def.inc"	; internal clock 8MHz, ATTINY4313
.LIST

		.EQU presc = 0			; clock prescaler value (Fcpu = clock / [2^presc])
			.IF presc > 8
				.ERROR "prescaler have to be set at values between 0 - 8!"
			.ENDIF
		.EQU graph_buff = 5		; graphics frame buffer size (1 Byte = 1 Graph line)
		.EQU char_buff = 20		; characters buffer size (1 Byte = 1 Character)

		.EQU v_internal_L = GPIOR0
		.EQU v_internal_H = GPIOR1
		.EQU v_screen = GPIOR2
		; v_screen parameters - [R] [6] [5] [4] [3] [2] [1] [0]
		; 0 - Screen mode ( 0 - text, 1 - graphical )
		; 1 - Turn off screen ( 0 - active, 1 - inactive ) - Also disables screen update routines, an frees buffer memory for other use
		; 2 - Invert screen ( 0 - normal, 1 - inverted )
		; 3 - V-scroll Left ( 0 - no scroll, 1 - scroll )
		; 4 - V-scroll Right ( 0 - no scroll, 1 - scroll )
		; 5 - Reserved for future use
		; 6 - Reserved for future use
		; R - Reserved for future use

		; TODO char buff to graph, and v scroll

		.DSEG
		.ORG	SRAM_START
v_graph_frame:		.BYTE	graph_buff		;	graphical frame buffer size
v_char_buffer:		.BYTE	char_buff		;	character buffer size
;v_internal:			.BYTE	2				;	variable contains number of timer tics, usable as timestamp. Two Byte wide
;2 Byte internal from ram, moved to GPIOR0/1 (Less ram usage)
v_scroll_speed:		.BYTE	1				;	variable contains screen shift speed
v_scroll_amount:	.BYTE	1				;	variable contains amount of screen moves (with shift)
v_frame_position:	.BYTE	1				;	variable contains frame buffer shift (start of buffer)

		.CSEG
		.ORG	$0000
		RJMP	I_RESET_ROUTINE			;	0x0000
		RJMP	unexpected_routine		;	0x0001
		RJMP	unexpected_routine		;	0x0002
		RJMP	unexpected_routine		;	0x0003
		RJMP	I_TIMER1_COMPA_ROUTINE	;	0x0004
		RJMP	unexpected_routine		;	0x0005
		RJMP	unexpected_routine		;	0x0006
		RJMP	I_UART_RX_ROUTINE		;	0x0007
		RJMP	I_UART_UDRE_ROUTINE		;	0x0008
		RJMP	I_UART_TX_ROUTINE		;	0x0009
		RJMP	unexpected_routine		;	0x000A
		RJMP	unexpected_routine		;	0x000B
		RJMP	unexpected_routine		;	0x000C
		RJMP	I_TIMER0_COMPA_ROUTINE	;	0x000D
		RJMP	unexpected_routine		;	0x000E
		RJMP	unexpected_routine		;	0x000F
		RJMP	unexpected_routine		;	0x0010
		RJMP	I_EEPRDY_ROUTINE		;	0x0011
		RJMP	I_WDT_ROUTINE			;	0x0012
		RJMP	unexpected_routine		;	0x0013
		RJMP	I_PCINT2_ROUTINE		;	0x0014

I_RESET_ROUTINE:
		CLI
		LDI	Xl, LOW(RAMEND)
		OUT	SPL, Xl
		LDI Xh, HIGH(RAMEND)
		OUT SPh, Xh
		CLR r16
		ADIW X, 1
	ram_clr_loop:
		ST - X, r16
		CPI Xl, LOW(SRAM_START)
		BRNE ram_clr_loop

MAIN:
		RCALL SETUP

	main_loop:
		RJMP main_loop

EEPROM_WRITE:							; r17 - address, r18 - data
		SBIC EECR, EEPE
		RJMP EEPROM_WRITE

		CLR r16
		OUT EECR, r16

		OUT EEAR, r17
		OUT EEDR, r18

		SBI EECR, EEMPE
		SBI EECR, EEPE
		RET

EEPROM_READ:							; r17 - address, r18 - data
		SBIC EECR, EEPE
		RJMP EEPROM_READ

		OUT EEAR, r17
		
		SBI EECR, EERE
		IN r18, EEDR
		RET

SETUP:
		CLI		; Disable interrupts, for setup

		CLR r17
		RCALL EEPROM_READ	; OSCCAL VALUE IN EEPROM 0x00
		OUT OSCCAL, r18		; Write read value from EEPROM to OSCCAL register, to trim internal oscillator

		LDI r16, $80
		OUT	CLKPR, r16		; Enable clock prescaler change
		LDI	r16, presc
		ANDI r16, $0F		; Get only first half of byte
		OUT	CLKPR, r16		; Load new prescaler value

		LDI r16, $02
		OUT	PRR, r16		; Disable clock for USI

		LDI r16, $03
		OUT DIDR, r16		; Disable comparator inputs
		LDI r16, $80
		OUT ACSR, r16		; Disable comparator

		LDI	r16, $03
		OUT DDRA, r16		; Set up PINA0 and PINA1 as outputs
		SER r16
		OUT DDRB, r16		; Set entire PortB as outputs
		LDI	r16, $1E
		OUT DDRD, r16		; Set PIND0, PIND2-4 as outputs
		CLR	r16
		OUT	PORTA, r16		; Clear PortA
		OUT PORTB, r16		; Clear PortB
		LDI	r16, $60
		OUT	PORTD, r16		; Clear PortD and enable Pull-Up on PIND5 and PIND6
		
		SBI TCCR0A, $1		; Set WGM01 to one, Timer0 in CTC mode (2)
		LDI r16, $03
		OUT TCCR0B, r16		; Set CS01 and CS00 to one, select clk(io)/64 for Timer0
		LDI r16, $7C
		OUT OCR0A, r16		; Set OCR0A value to 124(dec)
		SBI TIMSK, $0		; Set OCIE0A to one, Timer/Counter0 Output Compare Match A Interrupt Enable

		ORI r16, $49
		OUT WDTCSR, r16		; Enable watchdog, 64ms

		;LDI r16, 51
		;LDI r17, $00
		;RCALL UART_INIT

		LDI r16, $01
		OUT GTCCR, r16

		SEI
		RET


;write_matrix:				; r18 : range
;		LDI r16, 0
;		LDI r19, 0
;		RCALL MATRIX_CHAR
;		RET

UART_INIT:					; r16:r17 : UBRR - (Fosc)/(((16*~U2X)+(8*U2X))*BAUD)-1
		OUT UBRRH, r17
		OUT UBRRL, r16

		LDI r16, (1<<RXEN)|(1<<TXEN)
		OUT UCSRB, r16

		LDI r16, (1<<USBS)|(1<<UCSZ0)
		OUT UCSRC, r16
		RET

UART_TRANSMIT:				; r16 : data in
		SBIS UCSRA, UDRE
		RJMP UART_TRANSMIT

		OUT UDR, r16
		RET

UART_RECIEVE:				; r16 : data out
		SBIS UCSRA, RXC
		RJMP UART_RECIEVE

		in r16, UDR
		RET

SLEEP_WELL:
		CLI
		
		;LDI r17, 0xF
		;OUT PRR, r17
		CLR r16
		OUT PORTA, r16
		OUT PORTB, r16
		OUT DDRA, r16
		OUT DDRB, r16
		OUT DDRD, r16
		LDI	r16, 0x60
		OUT	PORTD, r16

		LDI r17, 0x10
		OUT GIMSK, r17
		LDI r17, 0x10
		OUT GIFR, r17
		LDI r17, 0x40
		OUT TIFR, r17
		LDI r17, 0x40
		OUT PCMSK2, r17

		WDR
		IN r16, MCUSR
		IN r16, WDTCSR
		ORI r16, (1<<WDCE)|(1<<WDE)
		OUT WDTCSR, r16
		LDI r16, (0<<WDE)
		OUT WDTCSR, r16

		LDI r16, 3
		IN r16, MCUCR
		SBR r16, 0x70
		OUT MCUCR, r16

		SEI
		NOP
		SLEEP
		CLI

		IN r16, MCUCR
		CBR r16, 0x70
		OUT MCUCR, r16

		CLR r17
		OUT GIMSK, r17
		OUT PCMSK2, r17
		RCALL setup
		RET

DIVIDE_16div16_16mod16:									;a / b = c R d, (rh:rl) r17:r16 / r19:r18 = r17:r16 R r15:r14
		CLR r14
		SUB r15, r15
		LDI r20, 17
	div_1:
		ROL r16
		ROL r17
		DEC r20
		BRNE div_2
		RET
	div_2:
		ROL r14
		ROL r15
		SUB r14, r18
		SBC r15, r19
		BRCC div_3
		ADD r14, r18
		ADC r15, r19
		CLC
		RJMP div_1
	div_3:
		SEC
		RJMP div_1

MULTIPLY_8mul8_16:								;	a * b = c, r16 * r17 = r18:r17
		CLR	r18
		LDI	r19, 8
		LSR r17
	mul_1: 
		BRCC mul_2
		ADD	r18, r16
	mul_2: 
		ROR r18
		ROR r17
		DEC r19
		BRNE mul_1
		RET

SHIFT_LEFT:								;	r16 : input, output, r17 : shift
	lshift_1:
		TST r17
		BRNE PC + 2
		RET
		LSL r16
		DEC r17
		BRNE lshift_1
		RET

MATRIX_CHAR:							;	r16 : x, r18 : range, r19 : char [ASCII code]
		LDI ZL, LOW(alpha << 1)			;	[char = 0, range = x - fill matrix with 0's in x width]
		LDI ZH, HIGH(alpha << 1)		;	[char = 1 - fill with 1's in x width]
		CLR r15

		MOV r14, r16
		
		TST r19
		BRNE PC + 4
		CLR r20
		SET
		RJMP move_buff
		CPI r19, 1
		BRNE PC + 4
		SER r20
		SET
		RJMP move_buff

		SER r21
		CLT
		CPI r18, 5
		BRLO mul_char
		CLR r21

	mul_char:
		PUSH r18
		SUBI r19, 32
		MOV r16, r19
		LDI r17, 5
		RCALL MULTIPLY_8mul8_16
		ADD ZL, r17
		ADC ZH, r18
		POP r18

	move_buff:
		BLD r15, 0
		;LDI YL, LOW(frame)
		;LDI YH, HIGH(frame)
		ADD YL, r14
		ADC YH, r14
		SUB YH, r14

	update_loop:
		SBRS r15, 0
		LPM r20, Z+
		ST Y+, r20
		DEC r18
		BREQ loop_end
		SBRS r15, 0
		INC r21
		SEZ
		SBRS r15, 0
		CPI r21, 0xFF
		BREQ update_loop
		CPI r21, 5
		BRNE update_loop
		SBIW Z, 5
		CLR r21
		RJMP update_loop
	loop_end:
		CLT
		RET

/*reverse_byte:							;	r16 : input, output
		LDI r19, 1
		PUSH r19
		MOV r17, r16
		PUSH r17
		CLR r16
		LDI r18, 0x80
		PUSH r18
	rev_1:
		CPI r19, 0x10
		BRSH PC + 6
		LSL r17
		LSR r18
		CP r18, r19
		BRNE rev_1
		RJMP PC + 5 
		LSR r17
		LSL r18
		CP r19, r18
		BRNE rev_1		
		POP r18
		AND r17, r18
		OR r16, r17
		POP r17
		LSR r18
		POP r19
		LSL r19
		PUSH r19
		PUSH r17
		PUSH r18
		TST r18
		BRNE rev_1
		POP r18
		POP r17
		POP r19
		RET*/

MATRIX_TEXT:							;	r16 : x, Z : text pointer
		BRTS PC + 4						;	SREG(T) = 0 - ROM pointer
		LPM r19, Z+						;	SREG(T) = 1 - RAM pointer
		BRTC PC + 2						
		LD r19, Z+
		TST r19							
		BRNE PC + 3
		CLT
		RET
		LDI r18, 5
		PUSH Zh
		PUSH Zl
		BLD r20, 0
		PUSH r20
		PUSH r16
		RCALL MATRIX_CHAR
		POP r16
		POP r20
		BST r20, 0
		POP Zl
		POP Zh
		LDI r18, 6
		ADD r16, r18
		RJMP MATRIX_TEXT

;INT0_routine:
;		RETI

;INT1_routine:
;		RETI

I_TIMER1_COMPA_ROUTINE:
		RETI
		/*PUSH r14
		PUSH r16
		PUSH r17
		PUSH r18
		PUSH r19
		PUSH r20
		;	push remaining registers onto stack
		CLR r16
		OUT PORTA, r16
		OUT PORTB, r16
		IN r16, PORTD
		CBR r16, 0x1C
		OUT PORTD, r16
		
		LDS XH, internal + 1
		LDS XL, internal
		
		MOVW r16, XL
		LDI r18, 5
		CLR r19
		RCALL DIVIDE_16div16_16mod16		; r14 = internal % 5

		MOV r17, r14
		LDI r16, 1
		RCALL SHIFT_LEFT	; r16 = 1 << r17

		MOV r17, r14
		CPI r17, 2			; if(r17 < 2)
		BRLO PC + 5
		ORI r16, 0x63
		OUT PORTD, r16
		CLR r16
		OUT PORTA, r16
		OUT PORTA, r16

		LDS r16, start
		ADD r14, r16		; r14 += start

		LDI YL, LOW(frame)
		LDI YH, HIGH(frame)
		ADD YL, r14
		ADC YH, r14
		SUB YH, r14

		LD	r16, Y

		LDS r17, screen
		SBRC r17, 1
		COM r16
		OUT PORTB, r16		; PORTB = r16
		LDS r16, screen
		ANDI r16, 0xC0		; if(screen(7..6) != 0)
		BREQ PC + 2
		RCALL screen_shift
		ADIW XH:XL, 1

		MOVW r16, XL
		LDI r18, 0xFF
		LDI r19, 0x3F
		RCALL DIVIDE_16div16_16mod16
		TST r14
		BRNE PC + 4
		TST r15
		BRNE PC + 2			; go sleep when internal mod 0x3FFF = 0
		RCALL SLEEP_WELL
		STS internal + 1, XH
		STS internal, XL
		POP r20
		POP r19
		POP r18
		POP r17
		POP r16
		POP r14
		RETI
		*/
		/*
		LDS Xh, count
		LDS Xl, count + 1
		IN r16, PIND
		ANDI r16, 0x40
		BRNE PC + 2
		ADIW Xh:Xl, 1
		STS count, Xh
		STS count + 1, Xl
		
		LDS r17, offcount
		IN r16, PIND
		ANDI r16, 0x40
		BREQ off_jmp
		TST Xh
		BRNE clr_offcount
		TST Xl
		BREQ clr_offcount
		INC r17
		RJMP PC + 2
		clr_offcount:
		CLR r17
		STS offcount, r17
		off_jmp:

		CLR r16
		CPI r17, 100
		BRNE off_nclr
		STS press, r16
		STS button, r16
		STS count, r16
		STS count + 1, r16
		STS offcount, r16
		STS offcount + 1, r16
		off_nclr:
		SBIW Xh:Xl, 50
		BRMI no_press
		LDI r16, 1
		STS button, r16
		no_press:
		ADIW Xh:Xl, 50

		LDS r15, press
		IN r17, PIND
		ANDI r17, 0x40
		BRNE  
		TST r16
		BREQ
		TST Xh
		BRNE
		CPI Xl, 50
		BRLO
		LDI r16, 1
		CPI Xl, 0x11110100
		BRHI
		CPC Xh, r16
		BRHI
		CPI r15, 0x80
		BRHI
		INC r15

		CPI Xl, 0x11110100
		BRLO
		CPC Xh, r16
		BRLO
	screen_shift:
		LDS r18, move
		LDS r20, amount
		LDS r21, screen
		TST r20
		BREQ PC + 2
		RJMP screen_move
		STS move, r20
		LDS r21, screen
		CBR r21, 0xC0
		STS screen, r21
		RET

	screen_move:
		MOV r17, XH
		MOV r16, XL
		RCALL DIVIDE_16div16_16mod16
		TST r14
		BRNE PC + 3
		TST r15
		BREQ PC + 2
		RET

		LDS r19, amount
		LDS r16, start
		LDS r20, screen
		SBRC r20, 6
		INC r16
		SBRC r20, 7
		DEC r16
		DEC r19
		STS start, r16
		STS amount, r19
		RET

		*/

I_UART_RX_ROUTINE:
		RETI

I_UART_UDRE_ROUTINE:
		RETI

I_UART_TX_ROUTINE:
		RETI

I_EEPRDY_ROUTINE:
		RETI

I_WDT_ROUTINE:
		PUSH r16
		IN r16, WDTCSR
		ORI r16, 0x40
		OUT WDTCSR, r16
		POP r16
		RETI

I_PCINT2_ROUTINE:
		RETI

unexpected_routine:
		rjmp unexpected_routine

;hello:	.DB "Hello world!", 0, 0
;text1:	.DB "Tak", 0

		.ORG	(FLASHEND - 237)	;Program ends at 0x02F9 (tn2313[a]), 0x06F9 (tn4313)
alpha:	.DB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x79, 0x00, 0x00, \
			0x00, 0x70, 0x00, 0x70, 0x00, 0x14, 0x7F, 0x14, 0x7F, 0x14, \
			0x12, 0x2A, 0x7F, 0x2A, 0x24, 0x62, 0x64, 0x08, 0x13, 0x23, \
			0x36, 0x49, 0x55, 0x22, 0x05, 0x00, 0x50, 0x60, 0x00, 0x00, \
			0x00, 0x1C, 0x22, 0x41, 0x00, 0x00, 0x41, 0x22, 0x1C, 0x00, \
			0x08, 0x2A, 0x1C, 0x2A, 0x08, 0x08, 0x08, 0x3E, 0x08, 0x08, \
			0x00, 0x05, 0x06, 0x00, 0x00, 0x08, 0x08, 0x08, 0x08, 0x08, \
			0x00, 0x06, 0x06, 0x00, 0x00, 0x02, 0x04, 0x08, 0x10, 0x20, \
			0x3E, 0x45, 0x49, 0x51, 0x3E, 0x00, 0x21, 0x7F, 0x01, 0x00, \
			0x21, 0x43, 0x45, 0x49, 0x31, 0x42, 0x41, 0x51, 0x69, 0x46, \
			0x0C, 0x14, 0x24, 0x7F, 0x04, 0x72, 0x51, 0x51, 0x51, 0x4E, \
			0x1E, 0x29, 0x49, 0x49, 0x06, 0x40, 0x47, 0x48, 0x50, 0x60, \
			0x36, 0x49, 0x49, 0x49, 0x36, 0x30, 0x49, 0x49, 0x4A, 0x3C, \
			0x00, 0x36, 0x36, 0x00, 0x00, 0x00, 0x35, 0x36, 0x00, 0x00, \
			0x00, 0x08, 0x14, 0x22, 0x41, 0x14, 0x14, 0x14, 0x14, 0x14, \
			0x41, 0x22, 0x14, 0x08, 0x00, 0x20, 0x40, 0x45, 0x48, 0x30, \
			0x26, 0x49, 0x4F, 0x41, 0x3E, 0x3F, 0x44, 0x44, 0x44, 0x3F, \
			0x7F, 0x49, 0x49, 0x49, 0x36, 0x3E, 0x41, 0x41, 0x41, 0x22, \
			0x7F, 0x41, 0x41, 0x22, 0x1C, 0x7F, 0x49, 0x49, 0x49, 0x41, \
			0x7F, 0x48, 0x48, 0x40, 0x40, 0x3E, 0x41, 0x41, 0x45, 0x26, \
			0x7F, 0x08, 0x08, 0x08, 0x7F, 0x00, 0x41, 0x7F, 0x41, 0x00, \
			0x02, 0x01, 0x41, 0x7E, 0x40, 0x7F, 0x08, 0x14, 0x22, 0x41, \
			0x7F, 0x01, 0x01, 0x01, 0x01, 0x7F, 0x20, 0x10, 0x20, 0x7F, \
			0x7F, 0x10, 0x08, 0x04, 0x7F, 0x3E, 0x41, 0x41, 0x41, 0x3E, \
			0x7F, 0x48, 0x48, 0x48, 0x30, 0x3E, 0x41, 0x45, 0x42, 0x3D, \
			0x7F, 0x48, 0x4C, 0x4A, 0x31, 0x31, 0x49, 0x49, 0x49, 0x46, \
			0x40, 0x40, 0x7F, 0x40, 0x40, 0x7E, 0x01, 0x01, 0x01, 0x7E, \
			0x7C, 0x02, 0x01, 0x02, 0x7C, 0x7F, 0x02, 0x0C, 0x02, 0x7F, \
			0x63, 0x14, 0x08, 0x14, 0x63, 0x60, 0x10, 0x0F, 0x10, 0x60, \
			0x43, 0x45, 0x49, 0x51, 0x61, 0x00, 0x00, 0x7F, 0x41, 0x41, \
			0x20, 0x10, 0x08, 0x04, 0x02, 0x41, 0x41, 0x7F, 0x00, 0x00, \
			0x10, 0x20, 0x40, 0x20, 0x10, 0x01, 0x01, 0x01, 0x01, 0x01, \
			0x00, 0x40, 0x20, 0x10, 0x00, 0x02, 0x15, 0x15, 0x15, 0x0F, \
			0x7F, 0x09, 0x11, 0x11, 0x0E, 0x0E, 0x11, 0x11, 0x11, 0x02, \
			0x0E, 0x11, 0x11, 0x09, 0x7F, 0x0E, 0x15, 0x15, 0x15, 0x0C, \
			0x08, 0x3F, 0x48, 0x40, 0x20, 0x08, 0x14, 0x15, 0x15, 0x1E, \
			0x7F, 0x08, 0x10, 0x10, 0x0F, 0x00, 0x11, 0x5F, 0x01, 0x00, \
			0x02, 0x01, 0x11, 0x5E, 0x00, 0x00, 0x7F, 0x04, 0x0A, 0x11, \
			0x00, 0x41, 0x7F, 0x01, 0x00, 0x1F, 0x10, 0x0C, 0x10, 0x0F, \
			0x1F, 0x08, 0x10, 0x10, 0x0F, 0x0E, 0x11, 0x11, 0x11, 0x0E, \
			0x1F, 0x14, 0x14, 0x14, 0x08, 0x08, 0x14, 0x14, 0x0, 0xC1F, \
			0x1F, 0x08, 0x10, 0x10, 0x08, 0x09, 0x15, 0x15, 0x15, 0x02, \
			0x10, 0x7E, 0x11, 0x01, 0x02, 0x1E, 0x01, 0x01, 0x02, 0x1F, \
			0x1C, 0x02, 0x01, 0x02, 0x1C, 0x1E, 0x01, 0x06, 0x01, 0x1E, \
			0x11, 0x0A, 0x04, 0x0A, 0x11, 0x18, 0x05, 0x05, 0x05, 0x1E, \
			0x11, 0x13, 0x15, 0x19, 0x11, 0x00, 0x08, 0x36, 0x41, 0x00, \
			0x00, 0x00, 0x7F, 0x00, 0x00, 0x00, 0x41, 0x36, 0x08, 0x00, \
			0x04, 0x08, 0x08, 0x04, 0x08, 0
.exit