	DSK HASH

**************************************************
* Note:		
*
* TO DO:	Grab new header from VSDRIVE 
*			reset nonce to random? based on last H00?
*			proceed until new header appears, or n nonces hashed
*			Repeat.
**************************************************
* Variables
**************************************************

INPUT32				EQU $E0				;	DS	4				; 32-bit Accumulator
XREGISTER32			EQU	$E4				;	DS	4				; input 1 for XOR, etc (X)
YREGISTER32			EQU	$E8				;	DS	4				; input 2 for MAJ, etc (Y)
RESULT32			EQU	$EC				;	DS	4				; temp storage for various operations
	
CURRENTCHUNK		EQU	$FF				; chunk zero or one.
HASHPASS			EQU	$FE				; pass zero or one.

CURRENTMESSAGELO	EQU	$FC
CURRENTMESSAGEHI	EQU	$FD

S0					EQU $80
S1					EQU	$84

TEMP0				EQU	$88				; temp storage for various operations
TEMP1				EQU	$8C				; temp storage for various operations

HASHCACHED			EQU	$D0				; is the first pass already done, and the result cached in CACHEDHASH?
ITERATION			EQU	$D1				; Do 255 iterations on each header, then fetch new one from disk.

**************************************************
* Apple Standard Memory Locations
**************************************************
CLRLORES	EQU	$F832
LORES		EQU	$C050
TXTSET		EQU	$C051
MIXCLR		EQU	$C052
MIXSET		EQU	$C053
TXTPAGE1	EQU	$C054
TXTPAGE2	EQU	$C055
KEY			EQU	$C000
C80STOREOFF	EQU	$C000
C80STOREON	EQU	$C001
STROBE		EQU	$C010
SPEAKER		EQU	$C030
VBL			EQU	$C02E
RDVBLBAR	EQU	$C019		; not VBL (VBL signal low
WAIT		EQU	$FCA8 
RAMWRTAUX	EQU	$C005
RAMWRTMAIN	EQU	$C004
SETAN3		EQU	$C05E		; Set annunciator-3 output to 0
SET80VID	EQU	$C00D		; enable 80-column display mode (WR-only)
HOME		EQU	$FC58		; clear the text screen
VTAB		EQU	$FC22		; Sets the cursor vertical position (from CV)
COUT		EQU	$FDED		; Calls the output routine whose address is stored in CSW,
;COUTI		EQU	$fbf0								;  normally COUTI
CROUT		EQU	$FD8E 			; prints CR

STROUT		EQU	$DB3A			;Y=String ptr high, A=String ptr low
PRBYTE		EQU	$FDDA 			; print hex byte in A

ALTTEXT		EQU	$C055
ALTTEXTOFF	EQU	$C054


PB0			EQU	$C061		; paddle 0 button. high bit set when pressed.
PDL0		EQU	$C064		; paddle 0 value, or should I use PREAD?
PREAD		EQU	$FB1E

ROMINIT  	EQU	$FB2F
ROMSETKBD	EQU	$FE89
ROMSETVID	EQU	$FE93

ALTCHAR		EQU	$C00F		; enables alternative character set - mousetext

CH			EQU	$24			; cursor Horiz
CV			EQU	$25			; cursor Vert

WNDWDTH		EQU	$21			; Width of text window
WNDTOP		EQU	$22			; Top of text window

BELL   		EQU	$FF3A     	; Monitor BELL routine
;CROUT  		EQU	$FD8E     	; Monitor CROUT routine
;PRBYTE 		EQU	$FDDA     	; Monitor PRBYTE routine
MLI    		EQU	$BF00     	; ProDOS system call
OPENCMD		EQU	$C8			; OPEN command index
READCMD		EQU	$CA			; READ command index
CLOSECMD	EQU	$CC			; CLOSE command index


**************************************************
* START - sets up various fiddly zero page bits
**************************************************

				ORG $2000						; PROGRAM DATA STARTS AT $2000

				JSR HOME						; clear screen

				STA	$C050   					; rw:TXTCLR	; Set Lo-res page 1, mixed graphics + text
				STA	$C053   					; rw:MIXSET
				STA	$C054   					; rw:TXTPAGE1
				STA	$C056   					; rw:LORES

				JSR FILLSCREENFAST				; blanks screen to black.
				JSR SPLASHSCREEN				; fancy lo-res graphics

STARTMINING		JSR BLOAD						; load HEADER.BIN into HEADER

]noncebyte = 0
				LUP 4
				LDA H00 + ]noncebyte
				STA NONCE + ]noncebyte
]noncebyte = ]noncebyte+1
				--^				


				LDA #$00
				STA HASHCACHED					; clear cache status
				STA ITERATION					
				JSR FLIPCOIN

				; set text window to last 4 lines of GR screen.
				LDA #$14
				STA CV
				STA	WNDTOP
				JSR VTAB

**************************************************
*	SETUP
**************************************************
*
*	Initialize hash values:
*	(first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19):
*		See HTABLE
*	
*	Initialize array of round constants:
*	(first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311):
*		See KTABLE
*	
*	Pre-processing (Padding):
*	begin with the original message of length L bits (80*8 = 640bits)
*	append a single '1' bit (641bits)
*	means shifting everything over 1 bit to be 81 bytes
*	append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512 (640+1+K+64=1024 K=319)
*	append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits (append 0000000000000280)

**************************************************
*	Pre-processing (Padding):
**************************************************	
; Start with MESSAGE padded out to 1024bits (see MESSAGE below)

*	Process the message in successive 512-bit chunks:
*	break message into 512-bit chunks

*	80byte header yields 1024bit message, so chunks = 2
*	Cache result of first chunk, so subsequent passes are cache then hash.

PREPROCESS
				LDA #$00
				STA HASHPASS					; pass the first = 0
				STA CURRENTCHUNK				; chunk the first = 0
				
				LDA MESSAGELO
				STA	CURRENTMESSAGELO
				LDA MESSAGEHI
				STA	CURRENTMESSAGEHI

INITIALIZEHASH  								; for the 32 bytes in INITIALHASH, push them into H00-H07

INITIALHASHES	
]hashnumber		=	31
				LUP 32
				LDA INITIALHASH + ]hashnumber			
				STA H00 + ]hashnumber					
]hashnumber		=	]hashnumber - 1	
				--^			

*	for each chunk
*	    create a 64-entry message schedule array w[0..63] of 32-bit words
*	    (The initial values in w[0..63] don't matter, so many implementations zero them here)

*			See WTABLE				

*	    copy chunk into first 16 words w[0..15] of the message schedule array

COPYCHUNKS

CHECKCACHE

; if HASHCACHED == 1
; AND chunk=0 AND pass=0
; then read from CACHEDHASH

				LDA HASHCACHED					; has chunk0 pass0 already done?
				BEQ NOTCACHED
				
CACHEDONE		LDA HASHPASS					; pass = 0
				ORA CURRENTCHUNK				; chunk = 0
				BEQ CACHETOHASH

NOTCACHED			JMP NOCACHE

CACHETOHASH
]cachebyte = 0
				LUP 32
				LDA CACHEDHASH + ]cachebyte
				STA H00 + ]cachebyte
]cachebyte = ]cachebyte+1
				--^				
				JMP CHECKCHUNK
NOCACHE


				LDA CURRENTCHUNK				; which chunk?
				BNE NEXTCHUNK					; skip chunk0 if already done
				
				LDY #$3F						; Y = 63 to 0 on chunk 0, then 64 to 127 on chunk 1
COPYCHUNK0		LDA (CURRENTMESSAGELO),Y
				STA W00,Y
				DEY
				BPL	COPYCHUNK0					; if hasn't rolled over to FF, loop to copy next byte.

***** if I'm on second pass, only do chunk0
; HASHPASS = 1, add to CURRENTCHUNK?
				LDA HASHPASS
				STA CURRENTCHUNK
***** if I'm on second pass, only do chunk0
				
				JMP EXTENDWORDS					; done with chunk 0

NEXTCHUNK		


**** Only does this (second chunk) on first pass. So CURRENTMESSAGE always points to MESSAGE (never MESSAGE2)
]chunkbyte = 64
				LUP 64
COPYCHUNK1		LDA MESSAGE + ]chunkbyte					
				STA W00 - 64 + ]chunkbyte					; 
]chunkbyte = ]chunkbyte + 1				
				--^

**** Only does this (second chunk) on first pass.


*	    Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:

*	    for i from 16 to 63
*	        s0 = (w[i-15] rightrotate  7) xor (w[i-15] rightrotate 18) xor (w[i-15] rightshift  3)
*			s0 = (XREGISTER32) xor (YREGISTER32) xor (INPUT32)

EXTENDWORDS
				LDX #60								; 15*4

EXTEND				TXA
				CLC
				ADC #$04							; increment A = 16
				;;CMP #$40							; compare to 64*4

				BNE	EXTEND2							; done with EXTEND step (done through 63)
				JMP INITIALIZE
				
EXTEND2			TAX
				;;SEC									; set carry for subtract
				;;SBC	#$0F							; -15

				LDXWR15								; takes X as arg. load W[a-15] into XREGISTER32 and ROR32

RIGHTROTATEX7	LUP	6
				RIGHTROTATEX32						; ROR32 6 more times
				--^

				STA XREGISTER32		
				;;TAX32								; should store partial result at XREGISTER32
				
RIGHTROTATE18	RIGHTROTATEXY8									; copy from XREGISTER32 into YREGISTER32 and ROR32 9 times
				LUP 2
				RIGHTROTATEY32						; ROR32 2 more times
				--^
								
				STA YREGISTER32		
				;;TAY32								; should store partial result at YREGISTER32

														; X still = X*4
				LDAWR15									; load W[a-15] into INPUT32

RIGHTSHIFT3		LUP	2
				RIGHTSHIFT32						; shift right, ignore carry
				--^
													; store partial result in INPUT32

*	        s0 = (w[i-15] rightrotate  7) xor (w[i-15] rightrotate 18) xor (w[i-15] rightshift  3)
*			s0 = (XREGISTER32) xor (YREGISTER32) xor (INPUT32)

				XORAXY32T0

; A32 -> TEMP0
				;;STATEMP0								
			
*	        s1 := (w[i- 2] rightrotate 17) xor (w[i- 2] rightrotate 19) xor (w[i- 2] rightshift 10)
				;;SEC									; set carry for subtract
				;;SBC	#$02							; -02

				LDXWR2								; load W14 into XREGISTER32 and ROR32 17 times
				STA XREGISTER32		
									
RIGHTROTATE17			TXYR32							; copy XREGISTER32 to YREGISTER32 and ROR32

RIGHTROTATE2			RIGHTROTATEY32					; ROR32 1 more time
				STA YREGISTER32		
				;;TAY32							; should store partial result at YREGISTER32

													; ; X = X*4
				LDAWS248							; load W14 into INPUT32 and ROR32

RIGHTSHIFT10	;;RIGHTSHIFT8
				;;LUP 2
				RIGHTSHIFT24					; shift right, ignore carry
				;;--^
													; store partial result in INPUT32
*	        s1 := (w[i- 2] rightrotate 17) xor (w[i- 2] rightrotate 19) xor (w[i- 2] rightshift 10)
*	        s1 := (XREGISTER32) xor (YREGISTER32) xor (INPUT32)
*	        w[i] := w[i-16] + s0 + w[i-7] + s1
*	        w[i] := w[i-16] + TEMP0 + w[i-7] + INPUT32
*	        w[i] := w[i-16] + INPUT32 + w[i-7] + XREGISTER32
				CLC
				
				XORAXYADD24

				;;SEC
				;;SBC	#$10						; w[0]

				; load W00 into pointer, add with X32, store to X32
				LDWADDXX16					; takes X

				;;TAX32							; transfer to XREGISTER32

				;;SEC
				;;SBC	#$07						; w[09]

				; load W09 into pointer, add with X32
				; store result in w[i]
												
				LDWADDX7STA32					; takes X, store in W16

												
STOREW			;;LDWSTA32						; store in W16

				JMP EXTEND						; repeat until i=63

INITIALIZE
*	    Initialize working variables to current hash value:
*	    Va := h00
*	    Vb := h01
*	    Vc := h02
*	    Vd := h03
*	    Ve := h04
*	    Vf := h05
*	    Vg := h06
*	    Vh := h07

HASHTOV

]bytenumber	= 0		
			LUP	32
HTOV		LDA H00 + ]bytenumber
			STA VA + ]bytenumber
]bytenumber	= ]bytenumber + 1
			--^

**************************************************
*	MAIN LOOP. OPTIMIZE THIS.
**************************************************



COMPRESSION

*	    Compression function main loop:
*	    for i from 0 to 63

				LDA #$00
COMPRESS			TAX
					
*	        S1 := (e rightrotate 6) xor (e rightrotate 11) xor (e rightrotate 25)

				LDVLDXR32 4							; pointer to VE, ROR32

RIGHTROTATE06	LUP 5
				RIGHTROTATEX32					; shift right, ignore carry
				--^
				STA XREGISTER32
				
				;;TAX32							; result in XREGISTER32
				TXYR32

RIGHTROTATE11	LUP 4
				RIGHTROTATEY32					; shift right 5 more times=11, ignore carry
				--^
				STA YREGISTER32
				;;TAY32							; result in YREGISTER32
				
RIGHTROTATE25	RIGHTROTATEYA8
				LUP 5
				RIGHTROTATEA32					; shift right 14 more times=25, ignore carry
				--^
				
*	        S1 := (XREGISTER32) xor (YREGISTER32) xor (INPUT32)

				XORAXY32S1
				

;S1
				;;STAS1								; store INPUT32 in S1


**** CHOICE and MAJ always take the same 3 arguments - make macros
														
*	        ch := (e and f) xor ((not e) and g)
; CH in INPUT32
*	        temp1 := Vh + S1 + ch + k[i] + w[i] = TEMP0

				CHOICE32ADD						

; S1 + CH
				;;LDSADC32 4							; (S1 + ch) in INPUT32
				
; + VH
				LDVHADC32
				
				LDKADC32									; K[i] in pointer
; + K[i]
				
				LDWADCS0							; W[i] in pointer
; + W[i]
;				LDXADC32							; (S1 + ch + VH + k[i] + w[i]) in INPUT32
				
; = TEMP0
				;;STATEMP0							; store temp1 at TEMP0



*	        S0 := (a rightrotate 2) xor (a rightrotate 13) xor (a rightrotate 22)

				LDVLDXR32 0								; pointer to VA, ROR32

RIGHTROTATE02	;;LUP 2
				RIGHTROTATEX32					; ROR 2 times
				;;--^
				STA XREGISTER32
				
				;;TAX32							; result in XREGISTER32
				
RIGHTROTATE13	RIGHTROTATEXY8
				LUP 2
				RIGHTROTATEY32					; ROR 11 more times=13
				--^
				STA YREGISTER32
				;;TAY32							; result in YREGISTER32
				
RIGHTROTATE22	RIGHTROTATE8
				RIGHTROTATEA32					; ROR 9 more times=22

				
*	        S0 := (XREGISTER32) xor (YREGISTER32) xor (INPUT32)

				XORAXY32S0

;S0				
				;;STAS0								; store INPUT32 in S0


**** CHOICE and MAJ always take the same 3 arguments - make macros

*	        maj := (a and b) xor (a and c) xor (b and c)
*	        temp2 := S0 + maj
*	        temp2 := S0 + INPUT32
													; load A,B,C into A32,X32,Y32
				MAJ32ADDT1							; MAJ in INPUT32

													; load S0 into X32
;S0 -> X32
				;;LDA STABLELO						; takes X as argument
				;;STA $00	
				;;LDA STABLEHI	
				;;STA $01       		  				; now word/pointer at $0+$1 points to 32bit word at STABLE,X 
				;;LDX32								; S0 in XREGISTER32
				
				;;CLC
				;;ADC32								; TEMP2 in INPUT32	
				
;A32 -> TEMP1
				;;STATEMP1							; temp2 to TEMP1
				

				
ROTATE

*	        Vh := Vg
*	        Vg := Vf
*	        Vf := Ve

; Store VG in VH
			VXTOVY 6;7

			VXTOVY 5;6
			
			VXTOVY 4;5

*	        Ve := Vd + temp1

				LDVADDT0STA 3

;TEMP0 -> X32
				;;LDX TEMPLO						
				;;STX $00	
				;;LDX TEMPHI
				;;STX $01       		  				; now word/pointer at $0+$1 points to TEMP0

				;;LDXADC32

				;;LDVSTA 4



*	        Vd := Vc
*	        Vc := Vb
*	        Vb := Va

				VXTOVY 2;3

				VXTOVY 1;2

				VXTOVY 0;1

*	        Va := temp1 + temp2

;TEMP1 -> X32
				;;LDX TEMPLO+1						
				;;STX $00	
				;;LDX TEMPHI+1
				;;STX $01       		  		; now word/pointer at $0+$1 points to TEMP1

				;;LDX32						; load TEMP1 into XREGISTER32

;TEMP0 -> A32
				LDATEMP0ADD 0
				;;CLC
				;;ADC32

				;;LDVSTA 0

COMPRESSLOOP	TXA							; Round 0-63 from stack
				CLC
				ADC #$04
				;;CMP #$40
				BEQ ADDHASH				; checks to see if we can skip or pull from cache

				JMP COMPRESS
**************************************************
*	END MAIN LOOP. 

*	FINALIZE HASH AND OUTPUT.
**************************************************




ADDHASH
	
*	    Add the compressed chunk to the current hash value:
*	    h0 := h0 + Va
*	    h1 := h1 + Vb
*	    h2 := h2 + Vc
*	    h3 := h3 + Vd
*	    h4 := h4 + Ve
*	    h5 := h5 + Vf
*	    h6 := h6 + Vg
*	    h7 := h7 + Vh

]varbyte = 0
				LUP 8
				
				CLC
				LDA H00+3 + ]varbyte
				ADC VA+3 + ]varbyte
				STA H00+3 + ]varbyte
				
				LDA H00+2 + ]varbyte
				ADC VA+2 + ]varbyte
				STA H00+2 + ]varbyte
				
				LDA H00+1 + ]varbyte
				ADC VA+1 + ]varbyte
				STA H00+1 + ]varbyte
				
				LDA H00 + ]varbyte
				ADC VA + ]varbyte
				STA H00 + ]varbyte
]varbyte = ]varbyte + 4
				--^
				

; if HASHCACHED == 0
; AND chunk=0 AND pass=0
; then write to CACHEDHASH

CHECKCHUNK		LDA CURRENTCHUNK
				BNE CHECKPASS					; did I just do chunk 0? INC and go back and do second chunk.
				INC CURRENTCHUNK				; set to chunk 1				

				LDA HASHCACHED					; has chunk0 pass0 already done?
				BEQ HASHTOCACHE 				; otherwise

				JMP COPYCHUNKS					; 

CHECKPASS		LDA HASHPASS					; pass 0? set the message to the hash output and go again

				BEQ INCHASHPASS					; pass 1, skip to digest.
				
				JMP DIGEST
				
INCHASHPASS		INC HASHPASS					; 
				JMP HASHTOMESSAGE

HASHTOCACHE
]cachebyte = 0
				LUP 32
				LDA H00 + ]cachebyte
				STA CACHEDHASH + ]cachebyte
]cachebyte = ]cachebyte+1
				--^				

				INC HASHCACHED					; don't repeat.
				JMP COPYCHUNKS					; 

				

HASHTOMESSAGE
				
												; for each of 32 bytes, Y
												; load byte from H00,Y
												; store at MESSAGE2,Y
COPYHASH

]hashbyte = 31
				LUP 32
				LDA H00 + ]hashbyte
				STA MESSAGE2 + ]hashbyte
]hashbyte = ]hashbyte - 1
				--^

				LDA #<MESSAGE2
				STA	CURRENTMESSAGELO
				LDA #>MESSAGE2
				STA	CURRENTMESSAGEHI

******* only need one chunk for message2
				LDA #$00 
				STA CURRENTCHUNK
				JMP INITIALIZEHASH				; re-initializes the original sqrt hash values for pass 2					

DIGEST											; done the thing. 

				LDA #$06						;	set the memory location for line $14.
				STA $29							;	
				LDA #$50						;	
				STA $28							;	
				LDY #$00				; 0
				
PRNONCE
]hashbyte		=	0				
				LUP 4
				LDX NONCE + ]hashbyte							; load from table pointer	
				PRHEX							; PRBYTE - clobbers Y
													;**** ROLL MY OWN?
]hashbyte = ]hashbyte + 1
				--^

]noncebyte = 0
				LUP 4
				LDA H00 + ]noncebyte
				STA NONCE + ]noncebyte
]noncebyte = ]noncebyte+1
				--^				

				INC CV								; down one line
				INC CV								; down one line
				LDA #$00
				STA CH								; left cursor
				INC $29								; 0650 -> 0750
				LDA #$50						;	
				STA $28							;	


PRDIGEST		
				LDX H00				
				BEQ ZEROBYTE
				JMP PRBYTE1
ZEROBYTE							; if zero, spin the coin
				JSR FLIPCOIN
				
									; if the first byte is 00, maybe additional bytes are, too?
				
				LDA H00
]hashbyte		=	1	
				LUP 8				; 18 leading zeroes == 9 zero bytes at current difficulty (roughly)
				ORA H00 + ]hashbyte
]hashbyte = ]hashbyte + 1
				--^
				BEQ INTERESTING
				JMP PRBYTE1
				
INTERESTING		LDY #$00			; print the interesting result and then crash
				PRHEX							

]hashbyte		=	1				
				LUP 19
				LDX H00 + ]hashbyte				
				PRHEX							
												
]hashbyte = ]hashbyte + 1
				--^

				RTS			; drop to monitor if we found anything interesting.
				
				
				
				LDX H00				
PRBYTE1			LDY #$00				; 0
				PRHEX							
												



]hashbyte		=	1				
				LUP 19
				LDX H00 + ]hashbyte				
				PRHEX							
												
]hashbyte = ]hashbyte + 1
				--^


NEXTLINE			LDA #$D0
				STA $28				; $0750 to $07D0
				LDY #$00				; 0

]hashbyte		=	20				
				LUP 12
				LDX H00 + ]hashbyte				
				PRHEX							
												
]hashbyte = ]hashbyte + 1
				--^


				INC ITERATION
				BEQ DONEWORK 

				JMP PREPROCESS					; INC NONCE, start over.		

DONEWORK		

				JMP STARTMINING



**************************************************
*	macros (expanded at assembly time)
**************************************************

;;LDW			MAC
;;			LDA WTABLELO,X					; takes X as argument
;;			STA $00
;;			LDA WTABLEHI,X
;;			STA $01       		  			; now word/pointer at $0+$1 points to 32bit word at WTABLE,X 
;;            <<<            ; End of Macro

;;LDK			MAC
;;			LDA KTABLELO,X					; takes X as argument
;;			STA $00
;;			LDA KTABLEHI,X
;;			STA $01       		  			; now word/pointer at $0+$1 points to 32bit word at KTABLE,X 
;;            <<<            ; End of Macro

;	LDH			MAC
;				LDA HTABLELO,X					; takes X as argument
;				STA $00
;				LDA HTABLEHI,X
;				STA $01       		  			; now word/pointer at $0+$1 points to 32bit word at HTABLE,X 
;	            <<<            ; End of Macro

;;LDV			MAC
;;				LDA VTABLELO,X					; takes X as argument
;;				STA $00
;;				LDA VTABLEHI,X
;;				STA $01       		  			; now word/pointer at $0+$1 points to 32bit word at VTABLE,X 
;;	            <<<            ; End of Macro

;;LDVV		MAC
;;			LDA VTABLELO+]1					; takes X as argument
;;			STA $00
;;			LDA VTABLEHI+]1
;;			STA $01       		  			; now word/pointer at $0+$1 points to 32bit word at VTABLE,X 
;;            <<<            ; End of Macro



LDVLDXR32	MAC
			LDA VA + ]1 + ]1 + ]1 + ]1			; load from table pointer
			LSR
			STA XREGISTER32						; store in 32 bit "accumulator"

			LDA VA + ]1 + ]1 + ]1 + ]1 +1		; load from table pointer
			ROR
			STA XREGISTER32+1					; store in 32 bit "accumulator"
	
			LDA VA + ]1 + ]1 + ]1 + ]1 +2		; load from table pointer
			ROR
			STA XREGISTER32+2					; store in 32 bit "accumulator"

			LDA VA + ]1 + ]1 + ]1 + ]1 +3		; load from table pointer
			ROR
			STA XREGISTER32+3					; store in 32 bit "accumulator"

			LDA #$00							; accumulator to 0
			ROR								; CARRY into bit7
			ORA	XREGISTER32					; acccumulator bit7 into BIT31
	
            <<<            ; End of Macro

LDVADDT0STA	MAC
			CLC
			LDA VA + ]1 + ]1 + ]1 + ]1 +3		; load from table pointer
			ADC TEMP0 +3
			STA VA + 16 +3				; load from table pointer
	
			LDA VA + ]1 + ]1 + ]1 + ]1 +2		; load from table pointer
			ADC TEMP0 +2
			STA VA + 16 +2				; load from table pointer
	
			LDA VA + ]1 + ]1 + ]1 + ]1 +1		; load from table pointer
			ADC TEMP0 +1
			STA VA + 16 +1				; load from table pointer
	
			LDA VA + ]1 + ]1 + ]1 + ]1			; load from table pointer
			ADC TEMP0
			STA VA + 16				; load from table pointer

            <<<            ; End of Macro

LDVLDX		MAC
			LDA VA + ]1 + ]1 + ]1 + ]1 +3		; load from table pointer
			STA XREGISTER32+3					; store in 32 bit "accumulator"
	
			LDA VA + ]1 + ]1 + ]1 + ]1 +2		; load from table pointer
			STA XREGISTER32+2					; store in 32 bit "accumulator"
	
			LDA VA + ]1 + ]1 + ]1 + ]1 +1		; load from table pointer
			STA XREGISTER32+1					; store in 32 bit "accumulator"
	
			LDA VA + ]1 + ]1 + ]1 + ]1			; load from table pointer
			STA XREGISTER32						; store in 32 bit "accumulator"

            <<<            ; End of Macro


LDVSTA		MAC
			LDA INPUT32+3						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1 +3		; load from table pointer
	
			LDA INPUT32+2						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1 +2		; load from table pointer
	
			LDA INPUT32+1						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1 +1		; load from table pointer
	
			LDA INPUT32						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1		; load from table pointer

            <<<            ; End of Macro


VXTOVY		MAC										; rotate Vn to Vn-1
			
			LDA VA + ]1+ ]1+ ]1+ ]1 				; load from table pointer
			STA VA + ]2+ ]2+ ]2+ ]2					; store in table pointer
	
			LDA VA +  ]1+ ]1+ ]1+ ]1 + 1			; load from table pointer
			STA VA +  ]2+ ]2+ ]2+ ]2 + 1			; store in table pointer
	
			LDA VA +  ]1+ ]1+ ]1+ ]1 + 2			; load from table pointer
			STA VA +  ]2+ ]2+ ]2+ ]2 + 2			; store in table pointer
	
			LDA VA +  ]1+ ]1+ ]1+ ]1 + 3			; load from table pointer
			STA VA +  ]2+ ]2+ ]2+ ]2 + 3			; store in table pointer
            <<<            ; End of Macro



LDXWR15		MAC									; X indicates which W0x word to read from

				LDA W00 - 60,X						; load from table pointer
				LSR
				STA XREGISTER32							; store in 32 bit "accumulator"

				LDA W00 + 1 - 60,X						; load from table pointer
				ROR
				STA XREGISTER32+1						; store in 32 bit "accumulator"

				LDA W00 + 2 - 60,X						; load from table pointer
				ROR
				STA XREGISTER32+2						; store in 32 bit "accumulator"

				LDA W00 + 3 - 60,X						; load from table pointer
				ROR
				STA XREGISTER32+3						; store in 32 bit "accumulator"

				LDA #$00							; accumulator to 0
				ROR								; CARRY into bit7
				ORA	XREGISTER32						; acccumulator bit7 into BIT31

			<<<


LDXWR2		MAC									; X indicates which W0x word to read from

				LDA W00 + 2 - 8,X						; load from table pointer
				LSR
				STA XREGISTER32							; store in 32 bit "accumulator"

				LDA W00 + 3 - 8,X						; load from table pointer
				ROR
				STA XREGISTER32+1						; store in 32 bit "accumulator"

				LDA W00 - 8,X							; load from table pointer
				ROR
				STA XREGISTER32+2						; store in 32 bit "accumulator"

				LDA W00 + 1 - 8,X						; load from table pointer
				ROR
				STA XREGISTER32+3						; store in 32 bit "accumulator"

				LDA #$00							; accumulator to 0
				ROR								; CARRY into bit7
				ORA	XREGISTER32						; acccumulator bit7 into BIT31

			<<<


LDAWR15		MAC									; X indicates which W0x word to read from
			
			LDA W00 - 60,X							; load from table pointer
			LSR
			STA INPUT32							; store in 32 bit "accumulator"

			LDA W00 + 1 - 60,X						; load from table pointer
			ROR
			STA INPUT32+1							; store in 32 bit "accumulator"
	
			LDA W00 + 2 - 60,X						; load from table pointer
			ROR
			STA INPUT32+2							; store in 32 bit "accumulator"
	
			LDA W00 + 3 - 60,X						; load from table pointer
			ROR
	
			<<<
			

LDAW		MAC									; X indicates which W0x word to read from
			
			LDA W00 + 3,X						; load from table pointer
			STA INPUT32+3						; store in 32 bit "accumulator"
	
			LDA W00 + 2,X						; load from table pointer
			STA INPUT32+2						; store in 32 bit "accumulator"
	
			LDA W00 + 1,X						; load from table pointer
			STA INPUT32+1						; store in 32 bit "accumulator"
	
			LDA W00,X							; load from table pointer
			STA INPUT32							; store in 32 bit "accumulator"

			<<<
			

LDAWS248	MAC									; X indicates which W0x word to read from

			LDA W00 - 8,X							; load from table pointer
			LSR
			STA INPUT32+1						; store in 32 bit "accumulator"

			LDA W00 + 1 - 8,X					; load from table pointer
			ROR
			STA INPUT32+2						; store in 32 bit "accumulator"
	
			LDA W00 + 2 - 8,X					; load from table pointer
			ROR
	
			<<<
			

LDWSTA32	MAC									; store INPUT32 in W0x word

			LDA INPUT32+3						; load from 32 bit "accumulator"
			STA W00 + 3,X							; store in table pointer
	
			LDA INPUT32+2						; load from 32 bit "accumulator"
			STA W00 + 2,X							; store in table pointer
	
			LDA INPUT32+1						; load from 32 bit "accumulator"
			STA W00 + 1,X							; store in table pointer
	
			LDA INPUT32						; load from 32 bit "accumulator"
			STA W00,X							; store in table pointer

			<<<


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



LDATEMP0ADD		MAC				; puts 4 bytes from ($01,$00) into 32 bit "accumulator" INPUT32, clobbers A,Y

			CLC
			LDA TEMP0+3							; load from table pointer
			ADC INPUT32+3						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1 +3		; load from table pointer	
	
			LDA TEMP0+2							; load from table pointer
			ADC INPUT32+2						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1 +2		; load from table pointer
	
			LDA TEMP0+1							; load from table pointer
			ADC INPUT32+1						; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1 +1		; load from table pointer
	
			LDA TEMP0							; load from table pointer
			ADC INPUT32							; store in 32 bit "accumulator"
			STA VA + ]1 + ]1 + ]1 + ]1		; load from table pointer

            <<<            ; End of Macro
;/LDATEMP0


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



LDKADC32	MAC								; puts 4 bytes from K0n into 32 bit "accumulator" INPUT32, clobbers A,Y
			CLC
			LDA K00 + 3,X					; load from table pointer
			ADC INPUT32+3				; ADD with CARRY with OPERAND
			STA INPUT32+3					; output to INPUT32, overflow into carry
	
			LDA K00 + 2,X					; load from table pointer
			ADC INPUT32+2				; ADD with CARRY with OPERAND
			STA INPUT32+2					; output to INPUT32, overflow into carry
	
			LDA K00 + 1,X					; load from table pointer
			ADC INPUT32+1				; ADD with CARRY with OPERAND
			STA INPUT32+1					; output to INPUT32, overflow into carry
	
			LDA K00,X						;load from table pointer
			ADC INPUT32					; ADD with CARRY with OPERAND
			STA INPUT32						; output to INPUT32, overflow into carry

            <<<            ; End of Macro
;/LDKADC32


LDVHADC32	MAC								; puts 4 bytes from K0n into 32 bit "accumulator" INPUT32, clobbers A,Y
			CLC

			LDA VH+3					; load from table pointer
			ADC INPUT32+3				; ADD with CARRY with OPERAND
			STA INPUT32+3					; output to INPUT32, overflow into carry
	
			LDA VH+2					; load from table pointer
			ADC INPUT32+2				; ADD with CARRY with OPERAND
			STA INPUT32+2					; output to INPUT32, overflow into carry
	
			LDA VH+1					; load from table pointer
			ADC INPUT32+1				; ADD with CARRY with OPERAND
			STA INPUT32+1					; output to INPUT32, overflow into carry
	
			LDA VH						;load from table pointer
			ADC INPUT32					; ADD with CARRY with OPERAND
			STA INPUT32						; output to INPUT32, overflow into carry

            <<<            ; End of Macro
;/LDVHADC32


LDWADCS0	MAC							; puts 4 bytes from W0n into 32 bit "accumulator" INPUT32, clobbers A,Y
			CLC
			LDA W00 + 3,X					; load from table pointer
			ADC INPUT32+3				; ADD with CARRY with OPERAND
			STA TEMP0+3					; output to TEMP0, overflow into carry
	
			LDA W00 + 2,X					; load from table pointer
			ADC INPUT32+2				; ADD with CARRY with OPERAND
			STA TEMP0+2					; output to TEMP0, overflow into carry
	
			LDA W00 + 1,X					; load from table pointer
			ADC INPUT32+1				; ADD with CARRY with OPERAND
			STA TEMP0+1					; output to TEMP0, overflow into carry
	
			LDA W00,X						;load from table pointer
			ADC INPUT32					; ADD with CARRY with OPERAND
			STA TEMP0						; output to TEMP0, overflow into carry

            <<<            ; End of Macro
;/LDWADC


LDWADDXX16	MAC							; puts 4 bytes from W0n into 32 bit "accumulator" XREGISTER32, clobbers A,Y

			CLC
			LDA W00 + 3 - 64,X				; load from table pointer
			ADC XREGISTER32+3				; ADD with CARRY with OPERAND
			STA XREGISTER32+3				; output to INPUT32, overflow into carry
	
			LDA W00 + 2 - 64,X				; load from table pointer
			ADC XREGISTER32+2				; ADD with CARRY with OPERAND
			STA XREGISTER32+2				; output to INPUT32, overflow into carry
	
			LDA W00 + 1 - 64,X				; load from table pointer
			ADC XREGISTER32+1				; ADD with CARRY with OPERAND
			STA XREGISTER32+1				; output to INPUT32, overflow into carry
	
			LDA W00 - 64,X						;load from table pointer
			ADC XREGISTER32					; ADD with CARRY with OPERAND
			STA XREGISTER32					; output to INPUT32, overflow into carry

            <<<            ; End of Macro
;/LDWADDXX

LDWADDX7STA32	MAC							; puts 4 bytes from W0n into 32 bit "accumulator" INPUT32, clobbers A,Y

			CLC
			LDA W00 + 3 - 28,X				; load from table pointer
			ADC XREGISTER32+3				; ADD with CARRY with OPERAND
			STA W00 + 3,X							; store in table pointer
	
			LDA W00 + 2 - 28,X				; load from table pointer
			ADC XREGISTER32+2				; ADD with CARRY with OPERAND
			STA W00 + 2,X							; store in table pointer
	
			LDA W00 + 1 - 28,X				; load from table pointer
			ADC XREGISTER32+1				; ADD with CARRY with OPERAND
			STA W00 + 1,X							; store in table pointer
	
			LDA W00 - 28,X						;load from table pointer
			ADC XREGISTER32					; ADD with CARRY with OPERAND
			STA W00,X							; store in table pointer

            <<<            ; End of Macro
;/LDWADDX

LDWADDX	MAC								; puts 4 bytes from W0n into 32 bit "accumulator" INPUT32, clobbers A,Y

			TXA
			ASL
			ROL
			TAX								; x=x*4

			LDA W00 + 3,X					; load from table pointer
			ADC XREGISTER32+3				; ADD with CARRY with OPERAND
			STA INPUT32+3					; output to INPUT32, overflow into carry
	
			LDA W00 + 2,X					; load from table pointer
			ADC XREGISTER32+2				; ADD with CARRY with OPERAND
			STA INPUT32+2					; output to INPUT32, overflow into carry
	
			LDA W00 + 1,X					; load from table pointer
			ADC XREGISTER32+1				; ADD with CARRY with OPERAND
			STA INPUT32+1					; output to INPUT32, overflow into carry
	
			LDA W00,X						;load from table pointer
			ADC XREGISTER32					; ADD with CARRY with OPERAND
			STA INPUT32						; output to INPUT32, overflow into carry

            <<<            ; End of Macro
;/LDWADDX

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


*	Produce the final hash value (big-endian):
*	digest := hash := h0 append h1 append h2 append h3 append h4 append h5 append h6 append h7
*
**************************************************


INCNONCE	MAC				
			INC	NONCE+3							; rolled to zero, do next byte up
			BNE NONCEDONE				
			INC	NONCE+2							; rolled to zero, do next byte up
			BNE NONCEDONE				
			INC	NONCE+1							; rolled to zero, do next byte up
			BNE NONCEDONE				
			INC	NONCE							; rolled to zero, do next byte up
			BNE NONCEDONE				
			JMP DONEWORK						; done? seriously?
NONCEDONE					
			<<<
;			RTS




MAJ32ADDT1	MAC	
			; majority function. Takes INPUT32, XREGISTER32 and YREGISTER32, returns with result in INPUT32, clobbers AXY
			
			; load VA,VB,VC into AXY

			CLC
			LDA VA + 3				; A and Y, result to RESULT32,4
			AND VC + 3
			STA RESULT32+4
			LDA VB + 3				; X and Y, result to RESULT32,5
			AND VC + 3
			STA RESULT32+5
			; RESULT32,3 xor RESULT32,4 xor RESULT32,5
			LDA VA + 3
			AND VB + 3				; A and X, result to RESULT32,3
			EOR RESULT32+4
			EOR RESULT32+5
			ADC S0 + 3
			STA INPUT32 + 3

			LDA VA + 2				; A and Y, result to RESULT32,4
			AND VC + 2
			STA RESULT32+4
			LDA VB + 2				; X and Y, result to RESULT32,5
			AND VC + 2
			STA RESULT32+5
			; RESULT32,3 xor RESULT32,4 xor RESULT32,5
			LDA VA + 2
			AND VB + 2				; A and X, result to RESULT32,3
			EOR RESULT32+4
			EOR RESULT32+5
			ADC S0 + 2
			STA	INPUT32+2

			LDA VA + 1				; A and Y, result to RESULT32,4
			AND VC + 1
			STA RESULT32+4
			LDA VB + 1				; X and Y, result to RESULT32,5
			AND VC + 1
			STA RESULT32+5
			; RESULT32,3 xor RESULT32,4 xor RESULT32,5
			LDA VA + 1
			AND VB + 1				; A and X, result to RESULT32,3
			EOR RESULT32+4
			EOR RESULT32+5
			ADC S0 + 1
			STA	INPUT32+1

			LDA VA + 0				; A and Y, result to RESULT32,4
			AND VC + 0
			STA RESULT32+4
			LDA VB + 0				; X and Y, result to RESULT32,5
			AND VC + 0
			STA RESULT32+5
			; RESULT32,3 xor RESULT32,4 xor RESULT32,5
			LDA VA + 0
			AND VB + 0				; A and X, result to RESULT32,3
			EOR RESULT32+4
			EOR RESULT32+5
			ADC S0
			STA	INPUT32+0		

			<<<            ; End of Macro

;/MAJ32



			; choice function. Takes INPUT32 and XREGISTER32, adds S1, returns with result in INPUT32, clobbers AXY
CHOICE32ADD	MAC	
			; ch := (A and X) xor ((not A) and Y)
			; (RESULT32,2) EOR ((not A) AND Y)
			; (RESULT32,2) EOR (RESULT32,1 AND Y)
			
			; if bit(A)=0, then bit(Y), else bit(X)
			
			CLC
			LDA VE + 3					; VE
			AND VF + 3				
			STA RESULT32+2				; A AND X to TEMP
			LDA VE + 3				; AND with (NOT A)
			EOR #$FF					; NOT A
			AND VG + 3					; VG
			EOR RESULT32+2				; EOR (A AND X)
			ADC S1 + 3							; ADD with CARRY with OPERAND
			STA INPUT32 + 3
		
			LDA VE + 2					; VE
			AND VF + 2				
			STA RESULT32+2				; A AND X to TEMP
			LDA VE + 2				; AND with (NOT A)
			EOR #$FF					; NOT A
			AND VG + 2					; VG
			EOR RESULT32+2				; EOR (A AND X)
			ADC S1 + 2							; ADD with CARRY with OPERAND
			STA INPUT32 + 2
		
			LDA VE + 1					; VE
			AND VF + 1				
			STA RESULT32+2				; A AND X to TEMP
			LDA VE + 1				; AND with (NOT A)
			EOR #$FF					; NOT A
			AND VG + 1					; VG
			EOR RESULT32+2				; EOR (A AND X)
			ADC S1 + 1							; ADD with CARRY with OPERAND
			STA INPUT32 + 1
		
			LDA VE + 0					; VE
			AND VF + 0				
			STA RESULT32+2				; A AND X to TEMP
			LDA VE + 0				; AND with (NOT A)
			EOR #$FF					; NOT A
			AND VG + 0					; VG
			EOR RESULT32+2				; EOR (A AND X)
			ADC S1								; ADD with CARRY with OPERAND
			STA INPUT32 + 0

            <<<            ; End of Macro
			
;/CHOICE32

PRHEX	MAC			; replaces PRBYTE routine. still uses COUT. You're next, COUT. Watch your back.
					
					LDA HEXTOASCIIHI,X	; get ascii code for nibble "0"-"F"

					STA ($28),Y
					INY				; 1

					LDA HEXTOASCIILO,X	; get ascii code for nibble "0"-"F"

					STA ($28),Y
					INY
					
			<<<
;/PRHEX

**************************************************
*	subroutines
**************************************************
ROTATE1				LDA	ROTATE1LO		; Setup pointers to move memory
					STA	$3C				; $3C and $3D for source start
					LDA	ROTATE1HI
					STA	$3D
	
					LDA	ROTATE1LO
					STA	$3E				; $3E and $3F for source end
					LDA	ROTATE1HI
					CLC	
					ADC	#$04			; add $400 to start == end of graphic
					STA	$3F				; 
					JMP MOVEIMAGE

ROTATE2				LDA	ROTATE2LO		; Setup pointers to move memory
					STA	$3C				; $3C and $3D for source start
					LDA	ROTATE2HI
					STA	$3D
	
					LDA	ROTATE2LO
					STA	$3E				; $3E and $3F for source end
					LDA	ROTATE2HI
					CLC	
					ADC	#$04			; add $400 to start == end of graphic
					STA	$3F				; 
					JMP MOVEIMAGE

FLIPCOIN			JSR ROTATE1
					LDA #$30
					JSR WAIT
					JSR ROTATE2
					LDA #$30
					JSR WAIT
					;;JSR SPLASHSCREEN				; fancy lo-res graphics
					;;RTS ;fall through
;/FLIPCOIN					




SPLASHSCREEN		LDA	SPLASHLO		; Setup pointers to move memory
					STA	$3C				; $3C and $3D for source start
					LDA	SPLASHHI
					STA	$3D
	
					LDA	SPLASHLO
					STA	$3E				; $3E and $3F for source end
					LDA	SPLASHHI
					CLC	
					ADC	#$04			; add $400 to start == end of graphic
					STA	$3F				; 

MOVEIMAGE			LDA	#$00			; move graphic data to $8000
					STA	$42				; $42 and $43 for destination
					LDA	#$80
					STA	$43
					LDA	#$00			; Clear ACC, X,Y for smooth operation
					TAX	
					TAY	
					JSR	$FE2C    		; F8ROM:MOVE	; Do the memory move
						

										; display the data from $8000 at $400					
RESETVPTR			LDA	#$00			; Move titlepage from $8000 to $400 (screen)
					STA	$FE				; pointer for where we are at vertically on screen
					TAY					; Y-Reg used for indexing across (horiz) screen

VERTICALPTR			LDA	$FE				; pointer for where we are at vertically on screen
					JSR	$F847    		; F8ROM:GBASCALC
	
					LDA	$26
					STA	$FA				; $FA is our offset GBASL Byte (Source data titlepage)
		
					LDA	$27				; Add 04 w/ Carry to get to $8000 where graphic data is
					ADC	#$7C	
					STA	$FB				; $FB is our offset GBASH Byte (Source data titlepage)
							
					LDY #$07
GRABSTORAGE			LDA	($FA),Y			; Grab from storage
					STA	($26),Y			; Put to screen
					INY		
					CPY	#$21			; #$28 past the width of screen?
					BNE	GRABSTORAGE		; No?  Back for another round

					LDA	#$00
					TAX	
					TAY	
	
						
					INC	$FE				; Next line down vertically
					LDA	#$00
					TAX	
					TAY	
					LDA	$FE
					CMP	#$14			; #$18 bottom of screen?
					BNE	VERTICALPTR		; No? Go back and do next line down
					
					RTS					; We now return you to your regular programming

;/SPLASHSCREEN




FILLSCREENFAST		LDA #$00 
      				LDY #$78 

FILL1 				DEY
      				STA $400, Y
      				STA $480, Y
      				STA $500, Y
      				STA $580, Y
      				BNE FILL1

      				LDY #$50 			; #$78 for all 24 lines.

FILL2 				DEY
					STA $600, Y
					STA $680, Y
					STA $700, Y
					STA $780, Y
					BNE FILL2
					RTS




**************************************************
*	Load "HEADER" into memory
**************************************************

BLOAD   		JSR	OPEN    				;open "HEADER"
       			JSR READ
				JSR CLOSE
       			RTS            				;Otherwise done
				
OPEN 			JSR	MLI       				;Perform call
       			DB	OPENCMD    				;OPEN command number
       			DW	OPENLIST   				;Pointer to parameter list
       			BNE	ERROR     				;If error, display it
       			LDA REFERENCE
       			STA READLIST+1
       			STA CLOSELIST+1
       			RTS				

READ			JSR MLI
				DB	READCMD
				DW	READLIST
       			BNE ERROR					
				RTS

CLOSE			JSR MLI
				DB	CLOSECMD
				DW	CLOSELIST
       			BNE ERROR					
				RTS
				
ERROR  			CMP #$46					
				BNE PRINTERROR
				LDA #$30
				STA ENDNAME-1
				STA ENDNAME-2
				JMP OPEN
PRINTERROR		JSR	PRBYTE    				;Print error code
       			JSR	BELL      				;Ring the bell
       			JSR	CROUT     				;Print a carriage return
       			RTS				

OPENLIST		DB	$03						; parameter list for OPEN command
				DW	FILENAME
				DA	MLI-$400				; buffer snuggled up tight with PRODOS
REFERENCE		DB	$00						; reference to opened file
			
READLIST		DB	$04
				DB	$00						; REFERENCE written here after OPEN
				DB	<HEADER,>HEADER			; write to HEADER
				DB	$50,$00					; read as much as $ (80 bytes) - should error out with EOF before that.
TRANSFERRED		DB	$00,$00				

CLOSELIST		DB	$01
				DB	$00
				
FILENAME		DB	ENDNAME-NAME 			;Length of name
NAME    		ASC	'/HASH/HEADER.BIN' 			;followed by the name
ENDNAME 		EQU	*







**************************************************
* MESSAGE TO BE HASHED.
*
**************************************************
MESSAGELO			DB <MESSAGE
MESSAGEHI			DB >MESSAGE
	
MESSAGE				EQU *
	
HEADER				DS 76	;EQU *				; 80 bytes for header = L = 640 bits
	

*	What's being hashed is: version4 + previous block hash32 + merkel root32 + time4 + bits (target)4 + nonce4 = blockheader (80 bytes)

; 20000000e905eff72bb63f67b3abf7e0d930371814fad083000ce8640000000000000000cfbf4e0e035d175595d486a99705151b956c0a36d9febeb9ea7968ad38e54ead5d3b5f4a171f3a0800000000
;	[2019-07-26 16:15:11.585] Selecting pool 0 for work
;	[2019-07-26 16:15:11.586] Generated stratum merkle cfbf4e0e035d175595d486a99705151b956c0a36d9febeb9ea7968ad38e54ead
;	[2019-07-26 16:15:11.586] Generated stratum header 20000000e905eff72bb63f67b3abf7e0d930371814fad083000ce8640000000000000000cfbf4e0e035d175595d486a99705151b956c0a36d9febeb9ea7968ad38e54ead5d3b5f4a171f3a08000000000000008000000000000000000000000000000000000000000000000000000000
;	[2019-07-26 16:15:11.586] Work job_id 65e11 nonce2 5 ntime 5d3b5f4a
;	[2019-07-26 16:15:11.586] Generated target 00000000000000000000000000000000000000000000000080ff7f0000000000
;	[2019-07-26 16:15:11.586] Generated stratum work
 
;	VERSION			HEX 20,00,00,00	;	DS 4
;	PREVHASH		HEX e9,05,ef,f7,2b,b6,3f,67,b3,ab,f7,e0,d9,30,37,18,14,fa,d0,83,00,0c,e8,64,00,00,00,00,00,00,00,00	;	DS 32
;	MERKELROOT		HEX cf,bf,4e,0e,03,5d,17,55,95,d4,86,a9,97,05,15,1b,95,6c,0a,36,d9,fe,be,b9,ea,79,68,ad,38,e5,4e,ad	;	DS 32
;	TIMESTAMP		HEX 5d,3b,5f,4a	;	DS 4
;	TARGET			HEX 17,1f,3a,08	;	DS 4
NONCE			HEX 00,00,00,00	;	DS 4

									; then append one 1 bit
APPENDBIT		DB #$80				; 10000000 - 7 extra bits = 648 bits

									; append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
									; 648 + K + 64 = 1024
PADDINGBITS		DS 39				; K = 312 bits = 39 bytes
									;	append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
MESSAGELENGTH	HEX	00,00,00,00,00,00,02,80						
									; 	L = 640, in 8 bytes = 0,0,0,0,0,0,02,80

;nonce 0000 = 1A722F82830421E6F4655470C6565614144DD70202FC8C56D8E7C601060FBAD1
;NONCELO				DB	<NONCE
;NONCEHI				DB	>NONCE








JOBID 			ASC "65e11"

;	VERSION			HEX 01,00,00,00	;	DS 4
;	PREVHASH		HEX 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00	;	DS 32
;	MERKELROOT		HEX 3b,a3,ed,fd,7a,7b,12,b2,7a,c7,2c,3e,67,76,8f,61,7f,c8,1b,c3,88,8a,51,32,3a,9f,b8,aa,4b,1e,5e,4a	;	DS 32
;	TIMESTAMP		HEX 29,ab,5f,49	;	DS 4
;	TARGET			HEX ff,ff,00,1d	;	DS 4
;	NONCE			HEX 1d,ac,2b,7c	;	DS 4
;										; then append one 1 bit
;	APPENDBIT		DB #$80				; 10000000 - 7 extra bits = 648 bits
;										; append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
;										; 648 + K + 64 = 1024
;	PADDINGBITS		DS 39				; K = 312 bits = 39 bytes
;										;	append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
;	MESSAGELENGTH	HEX	00,00,00,00,00,00,02,80						
;										; 	L = 640, in 8 bytes = 0,0,0,0,0,0,02,80
;	JOBID 			ASC "0"	; GENESIS BLOCK




**** Message2 only needs one chunk! ****

;;MESSAGE2LO		DB <MESSAGE2
;;MESSAGE2HI		DB >MESSAGE2


MESSAGE2		DS 32				; new message content after first pass produces hash - 256 bits
									; then append one 1 bit
APPENDBIT2		DB #$80				; 10000000 - 7 extra bits = 264 bits

									; append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
									; 264 + K + 64 = 512 !!!! 1024
PADDINGBITS2	DS 23				; K = 184 bits = 87 bytes
									;	append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
MESSAGELENGTH2	HEX	00,00,00,00,00,00,01,00						
									; 	L = 256, in 8 bytes = 0,0,0,0,0,0,01,00







**************************************************
* Data Tables
*
**************************************************

		
;;TEMPLO			DB <TEMP0,<TEMP1
;;TEMPHI			DB >TEMP0,>TEMP1		

;;RESULT32LO		DB	<RESULT32
;;RESULT32HI		DB	>RESULT32

VA					DS	4
VB					DS	4
VC					DS	4
VD					DS	4
VE					DS	4
VF					DS	4
VG					DS	4
VH					DS	4

;	VTABLE				DA	VA,VB,VC,VD,VE,VF,VG,VH
;;VTABLELO			DB	<VA,<VB,<VC,<VD,<VE,<VF,<VG,<VH
;;VTABLEHI			DB	>VA,>VB,>VC,>VD,>VE,>VF,>VG,>VH


												
;    create a 64-entry message schedule array w[0..63] of 32-bit words
;    (The initial values in w[0..63] don't matter, so many implementations zero them here)

	DS \
	DS 64
W00					DS 4
W01					DS 4
W02					DS 4
W03					DS 4
W04					DS 4
W05					DS 4
W06					DS 4
W07					DS 4
W08					DS 4
W09					DS 4
W10					DS 4
W11					DS 4
W12					DS 4
W13					DS 4
W14					DS 4
W15					DS 4
W16					DS 4
W17					DS 4
W18					DS 4
W19					DS 4
W20					DS 4
W21					DS 4
W22					DS 4
W23					DS 4
W24					DS 4
W25					DS 4
W26					DS 4
W27					DS 4
W28					DS 4
W29					DS 4
W30					DS 4
W31					DS 4
W32					DS 4
W33					DS 4
W34					DS 4
W35					DS 4
W36					DS 4
W37					DS 4
W38					DS 4
W39					DS 4
W40					DS 4
W41					DS 4
W42					DS 4
W43					DS 4
W44					DS 4
W45					DS 4
W46					DS 4
W47					DS 4
W48					DS 4
W49					DS 4
W50					DS 4
W51					DS 4
W52					DS 4
W53					DS 4
W54					DS 4
W55					DS 4
W56					DS 4
W57					DS 4
W58					DS 4
W59					DS 4
W60					DS 4
W61					DS 4
W62					DS 4
W63					DS 4

;	WTABLE			DA	W00,W01,W02,W03,W04,W05,W06,W07,W08,W09
;					DA	W10,W11,W12,W13,W14,W15,W16,W17,W18,W19
;					DA	W20,W21,W22,W23,W24,W25,W26,W27,W28,W29
;					DA	W30,W31,W32,W33,W34,W35,W36,W37,W38,W39
;					DA	W40,W41,W42,W43,W44,W45,W46,W47,W48,W49
;					DA	W50,W51,W52,W53,W54,W55,W56,W57,W58,W59
;					DA	W60,W61,W62,W63

;;WTABLELO		DB	<W00,<W01,<W02,<W03,<W04,<W05,<W06,<W07,<W08,<W09
;;				DB	<W10,<W11,<W12,<W13,<W14,<W15,<W16,<W17,<W18,<W19
;;				DB	<W20,<W21,<W22,<W23,<W24,<W25,<W26,<W27,<W28,<W29
;;				DB	<W30,<W31,<W32,<W33,<W34,<W35,<W36,<W37,<W38,<W39
;;				DB	<W40,<W41,<W42,<W43,<W44,<W45,<W46,<W47,<W48,<W49
;;				DB	<W50,<W51,<W52,<W53,<W54,<W55,<W56,<W57,<W58,<W59
;;				DB	<W60,<W61,<W62,<W63

;;WTABLEHI		DB	>W00,>W01,>W02,>W03,>W04,>W05,>W06,>W07,>W08,>W09
;;				DB	>W10,>W11,>W12,>W13,>W14,>W15,>W16,>W17,>W18,>W19
;;				DB	>W20,>W21,>W22,>W23,>W24,>W25,>W26,>W27,>W28,>W29
;;				DB	>W30,>W31,>W32,>W33,>W34,>W35,>W36,>W37,>W38,>W39
;;				DB	>W40,>W41,>W42,>W43,>W44,>W45,>W46,>W47,>W48,>W49
;;				DB	>W50,>W51,>W52,>W53,>W54,>W55,>W56,>W57,>W58,>W59
;;				DB	>W60,>W61,>W62,>W63


;	Initialize hash values:
;	(first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19):

CACHEDHASH		DS 32					; storage for the first chunk of first pass. Won't change between nonce changes.

INITIALHASH		HEX	6a,09,e6,67			; need to keep this on hand to reset after hash pass 1.
				HEX	bb,67,ae,85
				HEX	3c,6e,f3,72
				HEX	a5,4f,f5,3a
				HEX	51,0e,52,7f
				HEX	9b,05,68,8c
				HEX	1f,83,d9,ab
				HEX	5b,e0,cd,19

H00				HEX	6a,09,e6,67
H01				HEX	bb,67,ae,85
H02				HEX	3c,6e,f3,72
H03				HEX	a5,4f,f5,3a
H04				HEX	51,0e,52,7f
H05				HEX	9b,05,68,8c
H06				HEX	1f,83,d9,ab
H07				HEX	5b,e0,cd,19

;	HTABLE			DA	H00,H01,H02,H03,H04,H05,H06,H07
;;HTABLELO		DB	<H00,<H01,<H02,<H03,<H04,<H05,<H06,<H07
;;HTABLEHI		DB	>H00,>H01,>H02,>H03,>H04,>H05,>H06,>H07


;	STABLE				DA	S0,S1
;;STABLELO			DB	<S0,<S1
;;STABLEHI			DB	>S0,>S1


;	Initialize array of round constants:
;	(first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311):

	DS \
K00				HEX	42,8a,2f,98
K01				HEX	71,37,44,91
K02				HEX	b5,c0,fb,cf
K03				HEX	e9,b5,db,a5
K04				HEX	39,56,c2,5b
K05				HEX	59,f1,11,f1
K06				HEX	92,3f,82,a4
K07				HEX	ab,1c,5e,d5
K08				HEX	d8,07,aa,98
K09				HEX	12,83,5b,01
K10				HEX	24,31,85,be
K11				HEX	55,0c,7d,c3
K12				HEX	72,be,5d,74
K13				HEX	80,de,b1,fe
K14				HEX	9b,dc,06,a7
K15				HEX	c1,9b,f1,74
K16				HEX	e4,9b,69,c1
K17				HEX	ef,be,47,86
K18				HEX	0f,c1,9d,c6
K19				HEX	24,0c,a1,cc
K20				HEX	2d,e9,2c,6f
K21				HEX	4a,74,84,aa
K22				HEX	5c,b0,a9,dc
K23				HEX	76,f9,88,da
K24				HEX	98,3e,51,52
K25				HEX	a8,31,c6,6d
K26				HEX	b0,03,27,c8
K27				HEX	bf,59,7f,c7
K28				HEX	c6,e0,0b,f3
K29				HEX	d5,a7,91,47
K30				HEX	06,ca,63,51
K31				HEX	14,29,29,67
K32				HEX	27,b7,0a,85
K33				HEX	2e,1b,21,38
K34				HEX	4d,2c,6d,fc
K35				HEX	53,38,0d,13
K36				HEX	65,0a,73,54
K37				HEX	76,6a,0a,bb
K38				HEX	81,c2,c9,2e
K39				HEX	92,72,2c,85
K40				HEX	a2,bf,e8,a1
K41				HEX	a8,1a,66,4b
K42				HEX	c2,4b,8b,70
K43				HEX	c7,6c,51,a3
K44				HEX	d1,92,e8,19
K45				HEX	d6,99,06,24
K46				HEX	f4,0e,35,85
K47				HEX	10,6a,a0,70
K48				HEX	19,a4,c1,16
K49				HEX	1e,37,6c,08
K50				HEX	27,48,77,4c
K51				HEX	34,b0,bc,b5
K52				HEX	39,1c,0c,b3
K53				HEX	4e,d8,aa,4a
K54				HEX	5b,9c,ca,4f
K55				HEX	68,2e,6f,f3
K56				HEX	74,8f,82,ee
K57				HEX	78,a5,63,6f
K58				HEX	84,c8,78,14
K59				HEX	8c,c7,02,08
K60				HEX	90,be,ff,fa
K61				HEX	a4,50,6c,eb
K62				HEX	be,f9,a3,f7
K63				HEX	c6,71,78,f2

;	KTABLE			DA	K00,K01,K02,K03,K04,K05,K06,K07,K08,K09
;					DA	K10,K11,K12,K13,K14,K15,K16,K17,K18,K19
;					DA	K20,K21,K22,K23,K24,K25,K26,K27,K28,K29
;					DA	K30,K31,K32,K33,K34,K35,K36,K37,K38,K39
;					DA	K40,K41,K42,K43,K44,K45,K46,K47,K48,K49
;					DA	K50,K51,K52,K53,K54,K55,K56,K57,K58,K59
;					DA	K60,K61,K62,K63

;;KTABLELO		DB	<K00,<K01,<K02,<K03,<K04,<K05,<K06,<K07,<K08,<K09
;;				DB	<K10,<K11,<K12,<K13,<K14,<K15,<K16,<K17,<K18,<K19
;;				DB	<K20,<K21,<K22,<K23,<K24,<K25,<K26,<K27,<K28,<K29
;;				DB	<K30,<K31,<K32,<K33,<K34,<K35,<K36,<K37,<K38,<K39
;;				DB	<K40,<K41,<K42,<K43,<K44,<K45,<K46,<K47,<K48,<K49
;;				DB	<K50,<K51,<K52,<K53,<K54,<K55,<K56,<K57,<K58,<K59
;;				DB	<K60,<K61,<K62,<K63

;;KTABLEHI		DB	>K00,>K01,>K02,>K03,>K04,>K05,>K06,>K07,>K08,>K09
;;				DB	>K10,>K11,>K12,>K13,>K14,>K15,>K16,>K17,>K18,>K19
;;				DB	>K20,>K21,>K22,>K23,>K24,>K25,>K26,>K27,>K28,>K29
;;				DB	>K30,>K31,>K32,>K33,>K34,>K35,>K36,>K37,>K38,>K39
;;				DB	>K40,>K41,>K42,>K43,>K44,>K45,>K46,>K47,>K48,>K49
;;				DB	>K50,>K51,>K52,>K53,>K54,>K55,>K56,>K57,>K58,>K59
;;				DB	>K60,>K61,>K62,>K63
				

; 80 ascii '0'
; 4c7f3da0386523b102328418c28d886bb9dc9c555671884e8fcc9bcba407e819
; 4C7F3DA0386523B102328418C28D886BB9DC9C555671884E8FCC9BCBA407E819 - boom. (1 pass)

; The quick brown fox jumps over the lazy dog - len 43
; d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592 - https://www.xorbin.com/tools/sha256-hash-calculator
; D7A8FBB307D7809469CA9ABCB0082E4F8D5651E46D3CDB762D02D0BF37C9E592 - https://docs.google.com/spreadsheets/d/1ipesRlJGtS_1Tm452lOueBEZzdgZkaH-AihtJDo55Sc/edit#gid=2107569783

; D7A8FBB307D7809469CA9ABCB0082E4F8D5651E46D3CDB762D02D0BF37C9E592 - confirmed (1 pass)


;0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c
; GENESIS BLOCK
;af42031e805ff493a07341e2f74ff58149d22ab9ba19f61343e2c86c71c5d66d  pass 1
;6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000  pass 2


;AF42031E805FF493A07341E2F74FF58149D22AB9BA19F61343E2C86C71C5D66D - pass 1
;6FE28C0AB6F1B372C1A6A246AE63F74F931E8365E15A089C68D6190000000000 - boom.


	DS \
HEXTOASCIILO			ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
				ASC "0123456789ABCDEF" 
HEXTOASCIIHI			ASC "0000000000000000" 
				ASC "1111111111111111" 
				ASC "2222222222222222" 
				ASC "3333333333333333" 
				ASC "4444444444444444" 
				ASC "5555555555555555" 
				ASC "6666666666666666" 
				ASC "7777777777777777" 
				ASC "8888888888888888" 
				ASC "9999999999999999" 
				ASC "AAAAAAAAAAAAAAAA" 
				ASC "BBBBBBBBBBBBBBBB" 
				ASC "CCCCCCCCCCCCCCCC" 
				ASC "DDDDDDDDDDDDDDDD" 
				ASC "EEEEEEEEEEEEEEEE" 
				ASC "FFFFFFFFFFFFFFFF" 

SPLASHLO			db	<SPLASHSCREENDATA
SPLASHHI			db	>SPLASHSCREENDATA

ROTATE1LO			db	<ROTATE1DATA
ROTATE1HI			db	>ROTATE1DATA

ROTATE2LO			db	<ROTATE2DATA
ROTATE2HI			db	>ROTATE2DATA

SPLASHSCREENDATA	HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,D0,90,9D,99,99,99,99,99,99,99,90,90,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,99,99,99,99,99,99,99,99,DD,FF,FF,FF
					HEX	99,99,99,99,99,FF,FF,FF,99,99,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,0D
					HEX	D9,99,99,99,9F,9F,9F,DF,FF,FF,9F,DF,FF,FF,9F,9F,9F,99,99,99,09,00,00,00,00,00,00,00,00,00
					HEX	FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,D0,9D,99,99,99,99,99,99,99,99
					HEX	99,99,99,99,99,90,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,99,99,99,99
					HEX	99,99,99,99,DD,FF,FF,FF,F9,F9,F9,F9,FD,FF,FF,9F,99,99,99,99,99,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,0D,D9,99,99,99,99,99,DD,FF,FF,99,DD,FF,FF,99,99,99,99,99,09,00,00
					HEX	00,00,00,00,00,00,00,00,FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,D0,9D,99,99
					HEX	99,99,99,DD,FF,FF,99,DD,FF,FF,99,99,99,99,99,90,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,DD,99,99,99,99,99,99,99,99,DD,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,99,99,99,99,99,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,0D,D9,99,99,99,9D,9F,9F,99,9D,9F,9F
					HEX	99,99,99,09,00,00,00,00,00,00,00,00,00,00,00,00,FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00
					HEX	00,00,00,00,DD,99,99,99,99,99,99,DD,FF,FF,99,DD,FF,FF,99,99,99,99,99,99,90,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,DD,99,99,99,99,99,99,99,99,DD,FF,FF,FF,9F,9F,9F,9F,9F,DF
					HEX	FF,FF,FF,99,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,0D,D9
					HEX	89,99,99,99,99,99,99,99,89,09,00,00,00,00,00,00,00,00,00,00,00,00,00,00,FF,FF,FF,FF,FF,FF
					HEX	FF,FF,00,00,00,00,00,00,00,00,00,DD,99,99,99,99,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,F9,99
					HEX	99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,99,99,99,99,99,99,99,99,DD,FF
					HEX	FF,FF,99,99,99,99,99,99,FF,FF,FF,99,99,99,89,00,00,00,00,00,00,00,DF,DF,DF,DF,DF,DF,DF,DF
					HEX	DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,A0,A0,A0,A0,A0
					HEX	A0,D5,FF,FF,FF,FF,FF,FF,FF,70,00,00,00,00,00,00,00,00,D0,9D,99,99,99,99,9F,FF,FF,FF,FF,FF
					HEX	FF,FF,FF,FF,FF,FF,FF,F9,99,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,99
					HEX	99,99,99,99,99,99,DD,FF,FF,FF,99,99,99,99,99,D9,FF,FF,FF,99,99,99,00,00,00,00,00,00,00,00
					HEX	1F,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0
					HEX	A0,A0,1F,A0,A0,A0,A0,A0,A0,A0,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,DD,99,99,99
					HEX	99,99,99,99,DD,FF,FF,FF,99,99,99,9D,DF,FF,FF,FF,99,99,99,99,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,DD,99,99,99,99,99,99,F9,FD,FF,FF,FF,F9,F9,F9,F9,FD,FF,FF,FF,9F,99,99,99
					HEX	00,00,00,00,00,00,00,00,1F,A0,D8,BE,A0,B0,B0,A0,D9,BE,A0,B0,B0,A0,D0,EC,EF,F4,BE,A0,CF,E6
					HEX	E6,A0,C3,EF,EC,BE,A0,B0,B9,A0,1F,A0,A0,A0,A0,A0,A0,A0,70,FF,FF,FF,FF,FF,FF,00,00,00,00,00
					HEX	00,00,00,D0,9D,99,99,99,99,99,99,99,DD,FF,FF,FF,99,99,99,99,99,FF,FF,FF,99,99,99,99,90,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,99,99,99,99,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF
					HEX	FF,FF,FF,9F,99,99,99,00,00,00,00,00,00,00,00,00,20,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF
					HEX	DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,20,A0,BF,AD,C8,E5,EC,F0,C7,FF,FF,FF
					HEX	FF,FF,FF,FF


ROTATE1DATA			HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,D0,DD,9D,99,99,99,99,90,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,99,99,99,DD,FF,99
					HEX	99,99,DD,FF,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	0D,DD,D9,99,9D,9F,DF,FF,DF,FF,9F,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,FF,FF,FF,FF
					HEX	FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,99,99,99,99,99,99,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,99,99
					HEX	99,DD,FF,F9,F9,FD,FD,9F,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,DD,DD,99,99,99,DD,FF,DD,FF,99,99,99,09,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,99,DD,FF,DD,FF
					HEX	99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD
					HEX	99,99,99,99,99,DD,FF,FF,FF,FF,FF,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,9D,9F,9D,9F,99,99,09,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,99
					HEX	DD,FF,DD,FF,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,DD,DD,99,99,99,99,99,DD,FF,9F,9F,DF,DF,FF,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,0D,DD,D9,99,99,99,99,09,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD
					HEX	99,99,D9,F9,FD,FF,FD,FF,F9,99,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,DD,DD,99,99,99,99,99,DD,FF,99,99,99,DD,FF,99,99,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF
					HEX	DF,DF,DF,DF,DF,A0,A0,A0,A0,A0,A0,D5,FF,FF,FF,FF,FF,FF,FF,80,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,DD,DD,99,99,DD,FF,FF,FF,FF,FF,FF,FF,99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,0D,DD,D9,99,99,99,99,DD,FF,99,99,99,DD,FF,99,99,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,1F,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0
					HEX	A0,A0,A0,A0,A0,A0,A0,A0,1F,A0,A0,A0,A0,A0,A0,A0,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,DD,DD,99,99,99,99,DD,FF,99,99,9D,DF,F9,99,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,D9,F9,FD,FF,F9,F9,FD,FD,FF,99,09,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,1F,A0,D8,BE,A0,B0,B0,A0,D9,BE,A0,B0,B0,A0,D0,EC,EF,F4,BE,A0,
					HEX	CF,E6,E6,A0,C3,EF,EC,BE,A0,B0,C6,A0,1F,A0,A0,A0,A0,A0,A0,A0,70,FF,FF,FF,FF,FF,FF,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,DD,DD,99,99,99,99,99,DD,FF,99,99,99,DD,FF,99,99,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,99,99,DD,FF,FF,FF,FF,FF,FF,FF
					HEX	99,99,00,00,00,00,00,00,00,00,00,00,00,00,00,20,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF
					HEX	DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,20,A0,BF,AD,C8,E5,EC,F0,C7,FF,FF,FF,FF,FF,FF,FF


ROTATE2DATA			HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,D0,DF,DF,DF,D0,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,DD,DD,DD,DD,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,FF,FF,FF,FF
					HEX	FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD
					HEX	DD,DD,DD,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,DD,DF,DF,DF,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DF
					HEX	DF,DF,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,DD,DD,DD,DD,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,0D,DF,DF,DF,0D,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,FF,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,DD,FD,FD,FD,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF
					HEX	DF,DF,DF,DF,DF,A0,A0,A0,A0,A0,A0,D5,FF,FF,FF,FF,FF,FF,FF,80,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,DD,DD,DD,DD,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,DD,DD,99,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,1F,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0,A0
					HEX	A0,A0,A0,A0,A0,A0,A0,A0,1F,A0,A0,A0,A0,A0,A0,A0,FF,FF,FF,FF,FF,FF,FF,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DF,DF,DF,99,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,1F,A0,D8,BE,A0,B0,B0,A0,D9,BE,A0,B0,B0,A0,D0,EC,EF,F4,BE,A0
					HEX	CF,E6,E6,A0,C3,EF,EC,BE,A0,B0,C6,A0,1F,A0,A0,A0,A0,A0,A0,A0,70,FF,FF,FF,FF,FF,FF,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,DD,DD,DD,99,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,DD,FD,FD,FD,99,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,20,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF
					HEX	DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,20,A0,BF,AD,C8,E5,EC,F0,C7,FF,FF,FF,FF,FF,FF,FF