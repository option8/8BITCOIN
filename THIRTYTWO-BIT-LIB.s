
**************************************************
* Note:		
*
**************************************************
* Variables
**************************************************

INPUT32				EQU $E0				;	DS	4				; 32-bit Accumulator
XREGISTER32			EQU	$E4				;	DS	4				; input 1 for XOR, etc (X)
YREGISTER32			EQU	$E8				;	DS	4				; input 2 for MAJ, etc (Y)
RESULT32			EQU	$EC				;	DS	4				; temp storage for various operations
	
S0					EQU $80
S1					EQU	$84

TEMP0				EQU	$88				; temp storage for various operations
TEMP1				EQU	$8C				; temp storage for various operations

**************************************************
*	macros (expanded at assembly time)
**************************************************


STA32		MAC				; puts 4 bytes from 32 bit "accumulator" INPUT32 into ($01,$00), clobbers A,Y

			LDY #$03							
			LDA INPUT32+3						; load from 32 bit "accumulator"
			STA ($0),Y							; store in table pointer
	
			LDY #$02							
			LDA INPUT32+2						; load from 32 bit "accumulator"
			STA ($0),Y							; store in table pointer
	
			LDY #$01							
			LDA INPUT32+1						; load from 32 bit "accumulator"
			STA ($0),Y							; store in table pointer
	
			LDY #$00							
			LDA INPUT32						; load from 32 bit "accumulator"
			STA ($0),Y							; store in table pointer
	
            <<<            ; End of Macro



STAS1		MAC				; puts 4 bytes from 32 bit "accumulator" INPUT32 into S1

			LDA INPUT32+3						; load from 32 bit "accumulator"
			STA S1+3							; store in table pointer
	
			LDA INPUT32+2						; load from 32 bit "accumulator"
			STA S1+2							; store in table pointer
	
			LDA INPUT32+1						; load from 32 bit "accumulator"
			STA S1+1							; store in table pointer
	
			LDA INPUT32						; load from 32 bit "accumulator"
			STA S1							; store in table pointer
	
            <<<            ; End of Macro


STAS0		MAC				; puts 4 bytes from 32 bit "accumulator" INPUT32 into S0

			LDA INPUT32+3						; load from 32 bit "accumulator"
			STA S0+3							; store in table pointer
	
			LDA INPUT32+2						; load from 32 bit "accumulator"
			STA S0+2							; store in table pointer
	
			LDA INPUT32+1						; load from 32 bit "accumulator"
			STA S0+1							; store in table pointer
	
			LDA INPUT32						; load from 32 bit "accumulator"
			STA S0							; store in table pointer
	
            <<<            ; End of Macro

STATEMP1		MAC				; puts 4 bytes from 32 bit "accumulator" INPUT32 into TEMP0

			LDA INPUT32+3						; load from 32 bit "accumulator"
			STA TEMP1+3							; store in table pointer
	
			LDA INPUT32+2						; load from 32 bit "accumulator"
			STA TEMP1+2							; store in table pointer
	
			LDA INPUT32+1						; load from 32 bit "accumulator"
			STA TEMP1+1							; store in table pointer
	
			LDA INPUT32						; load from 32 bit "accumulator"
			STA TEMP1							; store in table pointer
	
            <<<            ; End of Macro



STATEMP0	MAC				; puts 4 bytes from 32 bit "accumulator" INPUT32 into TEMP0

			LDA INPUT32+3						; load from 32 bit "accumulator"
			STA TEMP0+3							; store in table pointer
	
			LDA INPUT32+2						; load from 32 bit "accumulator"
			STA TEMP0+2							; store in table pointer
	
			LDA INPUT32+1						; load from 32 bit "accumulator"
			STA TEMP0+1							; store in table pointer
	
			LDA INPUT32						; load from 32 bit "accumulator"
			STA TEMP0							; store in table pointer
	
            <<<            ; End of Macro




LDA32		MAC				; puts 4 bytes from ($01,$00) into 32 bit "accumulator" INPUT32, clobbers A,Y
			LDY #$03							
			LDA ($0),Y							; load from table pointer
			STA INPUT32+3						; store in 32 bit "accumulator"
	
			LDY #$02							
			LDA ($0),Y							; load from table pointer
			STA INPUT32+2						; store in 32 bit "accumulator"
	
			LDY #$01							
			LDA ($0),Y							; load from table pointer
			STA INPUT32+1						; store in 32 bit "accumulator"
	
			LDY #$00							
			LDA ($0),Y							; load from table pointer
			STA INPUT32						; store in 32 bit "accumulator"

            <<<            ; End of Macro
;/LDA32
LDX32		MAC				; puts 4 bytes from ($01,$00) into 32 bit "X register" XREGISTER32
			LDY #$03							
			LDA ($0),Y							; load from table pointer
			STA XREGISTER32+3						; store in 32 bit "X register"
	
			LDY #$02							
			LDA ($0),Y							; load from table pointer
			STA XREGISTER32+2						; store in 32 bit "X register"
	
			LDY #$01							
			LDA ($0),Y							; load from table pointer
			STA XREGISTER32+1						; store in 32 bit "X register"
	
			LDY #$00							
			LDA ($0),Y							; load from table pointer
			STA XREGISTER32						; store in 32 bit "X register"

            <<<            ; End of Macro
;/LDX32
LDY32		MAC				; puts 4 bytes from ($01,$00) into 32 bit "Y register" YREGISTER32
			LDY #$03							
			LDA ($0),Y							; load from table pointer
			STA YREGISTER32+3						; store in 32 bit "Y register"
	
			LDY #$02							
			LDA ($0),Y							; load from table pointer
			STA YREGISTER32+2						; store in 32 bit "Y register"
	
			LDY #$01							
			LDA ($0),Y							; load from table pointer
			STA YREGISTER32+1						; store in 32 bit "Y register"
	
			LDY #$00							
			LDA ($0),Y							; load from table pointer
			STA YREGISTER32						; store in 32 bit "Y register"

            <<<            ; End of Macro
;/LDY32

TAX32		MAC				
			LDA INPUT32+3								; load from INPUT32
			STA XREGISTER32+3								; store in 32 bit "X register"
			LDA INPUT32+2								; load from INPUT32
			STA XREGISTER32+2								; store in 32 bit "X register"
			LDA INPUT32+1								; load from INPUT32
			STA XREGISTER32+1								; store in 32 bit "X register"
			LDA INPUT32								; load from INPUT32
			STA XREGISTER32								; store in 32 bit "X register"
            <<<            ; End of Macro
;/TAX32

TAY32		MAC				
			LDA INPUT32+3								; load from INPUT32
			STA YREGISTER32+3								; store in 32 bit "Y register"
			LDA INPUT32+2								; load from INPUT32
			STA YREGISTER32+2								; store in 32 bit "Y register"
			LDA INPUT32+1								; load from INPUT32
			STA YREGISTER32+1								; store in 32 bit "Y register"
			LDA INPUT32								; load from INPUT32
			STA YREGISTER32								; store in 32 bit "Y register"
            <<<            ; End of Macro
;/TAY32

TXA32		MAC				
			LDA XREGISTER32+3								; load from 32 bit "X register"
			STA INPUT32+3								; store in INPUT32
			LDA XREGISTER32+2								; load from 32 bit "X register"
			STA INPUT32+2								; store in INPUT32
			LDA XREGISTER32+1								; load from 32 bit "X register"
			STA INPUT32+1								; store in INPUT32
			LDA XREGISTER32								; load from 32 bit "X register"
			STA INPUT32								; store in INPUT32
            <<<            ; End of Macro
;/TXA32

TYA32		MAC				
			LDA YREGISTER32+3							; load from 32 bit "Y register"
			STA INPUT32+3								; store in INPUT32
			LDA YREGISTER32+2							; load from 32 bit "Y register"
			STA INPUT32+2								; store in INPUT32
			LDA YREGISTER32+1							; load from 32 bit "Y register"
			STA INPUT32+1								; store in INPUT32
			LDA YREGISTER32							; load from 32 bit "Y register"
			STA INPUT32								; store in INPUT32
            <<<            ; End of Macro

;/TYA32

TYX32		MAC				
			LDA YREGISTER32+3							; load from 32 bit "Y register"
			STA XREGISTER32+3							; store in XREGISTER32
			LDA YREGISTER32+2							; load from 32 bit "Y register"
			STA XREGISTER32+2							; store in XREGISTER32
			LDA YREGISTER32+1							; load from 32 bit "Y register"
			STA XREGISTER32+1							; store in XREGISTER32
			LDA YREGISTER32							; load from 32 bit "Y register"
			STA XREGISTER32							; store in XREGISTER32
            <<<            ; End of Macro

;/TYX32



TXYR32		MAC				
			LSR								; load from 32 bit "X register"
			STA YREGISTER32							; store in YREGISTER32
			LDA XREGISTER32+1							; load from 32 bit "X register"
			ROR
			STA YREGISTER32+1							; store in YREGISTER32
			LDA XREGISTER32+2							; load from 32 bit "X register"
			ROR
			STA YREGISTER32+2							; store in YREGISTER32
			LDA XREGISTER32+3							; load from 32 bit "X register"
			ROR
			STA YREGISTER32+3							; store in YREGISTER32
			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	YREGISTER32							; acccumulator bit7 into BIT31

            <<<            ; End of Macro

;/TXYR32



RIGHTROTATEXY8	MAC							; rotate INPUT32 by a full byte
			STA YREGISTER32+1
			LDA XREGISTER32+3
			LSR
			STA YREGISTER32
			ROR YREGISTER32+1
			LDA XREGISTER32+1
			ROR
			STA YREGISTER32+2
			LDA XREGISTER32+2
			ROR
			STA YREGISTER32+3
			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	YREGISTER32						; acccumulator bit7 into BIT31
            <<<            ; End of Macro
;/RIGHTROTATEXY8

RIGHTROTATEYA8	MAC							; rotate INPUT32 by a full byte
			STA INPUT32+1
			LDA YREGISTER32+3
			LSR
			STA INPUT32
			ROR INPUT32+1
			LDA YREGISTER32+1
			ROR
			STA INPUT32+2
			LDA YREGISTER32+2
			ROR
			STA INPUT32+3
			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	INPUT32							; acccumulator bit7 into BIT31
            <<<            ; End of Macro
;/RIGHTROTATEYA8

RIGHTROTATE8	MAC							; rotate INPUT32 by a full byte
			STA	INPUT32+1
			LDA YREGISTER32+1
			STA INPUT32+2
			LDA YREGISTER32+2
			STA INPUT32+3
			LDA YREGISTER32+3
            <<<            ; End of Macro
;/RIGHTROTATE8

RIGHTSHIFT8	MAC									; rotate 32 bits right, 0->BIT31, clobbers AY
			LDY	#$00
			LDA INPUT32+2
			STA INPUT32+3
			LDA INPUT32+1
			STA INPUT32+2
			LDA INPUT32
			STA	INPUT32+1
			STY INPUT32
            <<<            ; End of Macro
;/RIGHTSHIFT8


RIGHTROTATEX32	MAC								; rotate 32 bits right, BIT0->BIT31, clobbers AY
			RIGHTSHIFTX32

			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	XREGISTER32							; acccumulator bit7 into BIT31

            <<<            ; End of Macro
;/RIGHTROTATEX32
		
												
RIGHTROTATEY32	MAC								; rotate 32 bits right, BIT0->BIT31, clobbers AY
			RIGHTSHIFTY32

			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	YREGISTER32							; acccumulator bit7 into BIT31

            <<<            ; End of Macro
;/RIGHTROTATEY32
		
												
RIGHTROTATE32	MAC								; rotate 32 bits right, BIT0->BIT31, clobbers AY
			RIGHTSHIFT32

			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	INPUT32								; acccumulator bit7 into BIT31

            <<<            ; End of Macro
;/RIGHTROTATE32
		
												
RIGHTROTATEA32	MAC								; rotate 32 bits right, BIT0->BIT31, clobbers AY
			RIGHTSHIFTA32

			LDA #$00							; accumulator to 0
			ROR									; CARRY into bit7
			ORA	INPUT32								; acccumulator bit7 into BIT31

            <<<            ; End of Macro
;/RIGHTROTATEA32
		
												
RIGHTSHIFTX32	MAC									; rotate 32 bits right, 0->BIT31, clobbers AY
			LSR
			STA XREGISTER32		
			ROR XREGISTER32+1							; put result into XREGISTER32
			ROR XREGISTER32+2							; put result into XREGISTER32
			ROR XREGISTER32+3							; put result into XREGISTER32
			
            <<<            ; End of Macro
;/RIGHTSHIFTX32


RIGHTSHIFTY32	MAC									; rotate 32 bits right, 0->BIT31, clobbers AY
			LSR
			STA YREGISTER32		
			ROR YREGISTER32+1							; put result into YREGISTER32
			ROR YREGISTER32+2							; put result into YREGISTER32
			ROR YREGISTER32+3							; put result into YREGISTER32
			
            <<<            ; End of Macro
;/RIGHTSHIFTY32


RIGHTSHIFT32	MAC									; rotate 32 bits right, 0->BIT31, clobbers AY
			LSR INPUT32		
			ROR INPUT32+1								; put result into INPUT32
			ROR INPUT32+2								; put result into INPUT32
			ROR									; put result into INPUT32
			
            <<<            ; End of Macro
;/RIGHTSHIFT32


RIGHTSHIFTA32	MAC									; rotate 32 bits right, 0->BIT31, clobbers AY
			LSR		
			STA INPUT32
			ROR INPUT32+1								; put result into INPUT32
			ROR INPUT32+2								; put result into INPUT32
			ROR INPUT32+3								; put result into INPUT32
			
            <<<            ; End of Macro
;/RIGHTSHIFTA32


RIGHTSHIFT24	MAC									; rotate 24 bits right, 0->BIT23, clobbers AY
			LSR INPUT32+1								; put result into INPUT32
			ROR INPUT32+2								; put result into INPUT32
			ROR									; put result into INPUT32
			
            <<<            ; End of Macro
;/RIGHTSHIFT24


ADC32	MAC	; Adds INPUT32 and XREGISTER32 with carry, if any, clobbers A,Y
			LDA INPUT32+3								; LDA byte
			ADC XREGISTER32+3								; ADD with CARRY with OPERAND
			STA INPUT32+3								; output to INPUT32, overflow into carry
			
			LDA INPUT32+2								; LDA byte
			ADC XREGISTER32+2								; ADD with CARRY with OPERAND
			STA INPUT32+2								; output to INPUT32, overflow into carry
			
			LDA INPUT32+1								; LDA byte
			ADC XREGISTER32+1								; ADD with CARRY with OPERAND
			STA INPUT32+1								; output to INPUT32, overflow into carry
			
			LDA INPUT32								; LDA byte
			ADC XREGISTER32								; ADD with CARRY with OPERAND
			STA INPUT32								; output to INPUT32, overflow into carry
			
            <<<            ; End of Macro
;/ADC32




LDXADC32	MAC									; adds INPUT32 with bytes from table 00,01

			CLC
			LDY #$03							
			LDA INPUT32+3						; LDA byte
			ADC ($0),Y							; ADD with CARRY with OPERAND
			STA INPUT32+3						; output to INPUT32, overflow into carry
	
			LDY #$02							
			LDA INPUT32+2						; LDA byte
			ADC ($0),Y							; ADD with CARRY with OPERAND
			STA INPUT32+2						; output to INPUT32, overflow into carry

			LDY #$01							
			LDA INPUT32+1						; LDA byte
			ADC ($0),Y							; ADD with CARRY with OPERAND
			STA INPUT32+1						; output to INPUT32, overflow into carry

			LDY #$00							
			LDA INPUT32							; LDA byte
			ADC ($0),Y							; ADD with CARRY with OPERAND
			STA INPUT32							; output to INPUT32, overflow into carry

            <<<            ; End of Macro
;/LDXADC32




LDSADC32	MAC									; adds INPUT32 with bytes from Stable

			CLC
			LDA INPUT32+3						; LDA byte
			ADC S0 + ]1 + 3							; ADD with CARRY with OPERAND
			STA INPUT32+3						; output to INPUT32, overflow into carry
	
			LDA INPUT32+2						; LDA byte
			ADC S0 + ]1 + 2							; ADD with CARRY with OPERAND
			STA INPUT32+2						; output to INPUT32, overflow into carry

			LDA INPUT32+1						; LDA byte
			ADC S0 + ]1 + 1							; ADD with CARRY with OPERAND
			STA INPUT32+1						; output to INPUT32, overflow into carry

			LDA INPUT32							; LDA byte
			ADC S0 + ]1							; ADD with CARRY with OPERAND
			STA INPUT32							; output to INPUT32, overflow into carry

            <<<            ; End of Macro
;/LDSADC32







AND32	MAC										; AND function, output to INPUT32, clobbers AY
			LDA INPUT32+3								; LDA byte
			AND XREGISTER32+3								; AND with OPERAND
			STA	INPUT32+3								; output to INPUT32
			
			LDA INPUT32+2								; LDA byte
			AND XREGISTER32+2								; AND with OPERAND
			STA	INPUT32+2								; output to INPUT32
			
			LDA INPUT32+2								; LDA byte
			AND XREGISTER32+1								; AND with OPERAND
			STA	INPUT32+1								; output to INPUT32
			
			LDA INPUT32								; LDA byte
			AND XREGISTER32								; AND with OPERAND
			STA	INPUT32								; output to INPUT32
			
            <<<            ; End of Macro
			
;/AND32

;	XOR32	MAC									; XOR function, output to INPUT32, clobbers AY
;				LDA INPUT32+3								; LDA byte
;				EOR XREGISTER32+3								; EOR with OPERAND
;				STA	INPUT32+3								; output to INPUT32
;				
;				LDA INPUT32+2								; LDA byte
;				EOR XREGISTER32+2								; EOR with OPERAND
;				STA	INPUT32+2								; output to INPUT32
;				
;				LDA INPUT32+1								; LDA byte
;				EOR XREGISTER32+1								; EOR with OPERAND
;				STA	INPUT32+1								; output to INPUT32
;				
;				LDA INPUT32								; LDA byte
;				EOR XREGISTER32								; EOR with OPERAND
;				STA	INPUT32								; output to INPUT32
;				
;	            <<<            ; End of Macro
;	;/XOR32





XORAXY32T0	MAC

			EOR XREGISTER32+3								; EOR with OPERAND
			EOR YREGISTER32+3								; EOR with OPERAND
			STA	TEMP0+3									; output to TEMP0
			
			LDA INPUT32+2								; LDA byte
			EOR XREGISTER32+2								; EOR with OPERAND
			EOR YREGISTER32+2								; EOR with OPERAND
			STA	TEMP0+2									; output to TEMP0
			
			LDA INPUT32+1								; LDA byte
			EOR XREGISTER32+1								; EOR with OPERAND
			EOR YREGISTER32+1								; EOR with OPERAND
			STA	TEMP0+1									; output to INPUT32
			
			LDA INPUT32								; LDA byte
			EOR XREGISTER32								; EOR with OPERAND
			EOR YREGISTER32								; EOR with OPERAND
			STA	TEMP0								; output to TEMP0
	

			<<<            ; End of Macro
;/XORAXY32            


XORAXY32S1	MAC

			EOR XREGISTER32								; EOR with OPERAND
			EOR YREGISTER32								; EOR with OPERAND
			STA	S1								; output to INPUT32
			
			LDA INPUT32+1								; LDA byte
			EOR XREGISTER32+1								; EOR with OPERAND
			EOR YREGISTER32+1								; EOR with OPERAND
			STA	S1+1									; output to INPUT32
			
			LDA INPUT32+2								; LDA byte
			EOR XREGISTER32+2								; EOR with OPERAND
			EOR YREGISTER32+2								; EOR with OPERAND
			STA	S1+2									; output to INPUT32

			LDA INPUT32+3								; LDA byte
			EOR XREGISTER32+3								; EOR with OPERAND
			EOR YREGISTER32+3								; EOR with OPERAND
			STA	S1+3									; output to INPUT32

			<<<            ; End of Macro
;/XORAXY32S1


XORAXY32S0	MAC
				
			EOR XREGISTER32								; EOR with OPERAND
			EOR YREGISTER32								; EOR with OPERAND
			STA	S0								; output to INPUT32

			LDA INPUT32+1								; LDA byte
			EOR XREGISTER32+1								; EOR with OPERAND
			EOR YREGISTER32+1								; EOR with OPERAND
			STA	S0+1									; output to INPUT32

			LDA INPUT32+2								; LDA byte
			EOR XREGISTER32+2								; EOR with OPERAND
			EOR YREGISTER32+2								; EOR with OPERAND
			STA	S0+2									; output to INPUT32

			LDA INPUT32+3								; LDA byte
			EOR XREGISTER32+3								; EOR with OPERAND
			EOR YREGISTER32+3								; EOR with OPERAND
			STA	S0+3									; output to INPUT32
			
			<<<            ; End of Macro
;/XORAXY32S0
			
			
XORAXYADD24	MAC
				
			EOR XREGISTER32+3								; EOR with OPERAND
			EOR YREGISTER32+3								; EOR with OPERAND
			ADC TEMP0+3									; ADD with CARRY with OPERAND
			STA	XREGISTER32+3								; output to XREGISTER32
			
			LDA INPUT32+2								; LDA byte
			EOR XREGISTER32+2								; EOR with OPERAND
			EOR YREGISTER32+2								; EOR with OPERAND
			ADC TEMP0+2									; ADD with CARRY with OPERAND
			STA	XREGISTER32+2								; output to XREGISTER32
			
			LDA INPUT32+1								; LDA byte
			EOR XREGISTER32+1								; EOR with OPERAND
			EOR YREGISTER32+1								; EOR with OPERAND
			ADC TEMP0+1
			STA	XREGISTER32+1								; output to XREGISTER32

			LDA XREGISTER32								; EOR with OPERAND
			EOR YREGISTER32								; EOR with OPERAND
			ADC TEMP0									; ADD with CARRY with OPERAND
			STA	XREGISTER32								; output to XREGISTER32
						
            <<<            ; End of Macro
;/XORAXYADD24            






