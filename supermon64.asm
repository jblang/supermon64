; ********************************
; * SUPERMON+ 64 JIM BUTTERFIELD *
; * V1.2   AUGUST 20 1985        *
; *                              *
; * REFORMATTED FOR 64TASS       *
; * DEC 2016 BY J.B. LANGSTON    *
; ********************************

; zero-page temp variables
; ------------------------
TMP0    = $C1       ; temporary pointer
TMP2    = $C3       ; temporary pointer

; kernal variables
; ----------------
SATUS   = $90       ; kernal i/o status word
FNLEN   = $B7       ; length of current filename
SADD    = $B9       ; current secondary address
FA      = $BA       ; current device number
FNADR   = $BB       ; pointer to current filename
NDX     = $C6       ; number of characters in keyboard buffer
KEYD    = $0277     ; keyboard buffer
BKVEC   = $0316     ; BRK vector

        *= $0100    ; store variables in tape error buffer

; 
ACMD    .FILL 1
LENGTH  .FILL 1
MNEMW   .FILL 3     ; 3 letter mnemonic
SAVX    .FILL 1     ; place to save X register
OPCODE  .FILL 1
UPFLG   .FILL 1     ; count up (bit 7 clear) or down (bit 7 set)
DIGCNT  .FILL 1     ; number of digits in number
INDIG   .FILL 1     ; numeric value of single digit
NUMBIT  .FILL 1     ; number of bits per digit, for parsing numbers
STASH   .FILL 2     ; place to stash values for later
U0AA0   .FILL 10
U0AAE   =*
STAGE   .FILL 30
ESTAGE  =*

        *= $0200    ; store more variables in basic line editor buffer

INBUFF  .FILL 40    ; 40-character input buffer
ENDIN   =*          ; end of input buffer
PCH     .FILL 1     ; program counter high byte
PCL     .FILL 1     ; program counter low byte
SR      .FILL 1     ; status register
ACC     .FILL 1     ; accumulator
XR      .FILL 1     ; X register
YR      .FILL 1     ; Y register
SP      .FILL 1     ; stack pointer
STORE   .FILL 2     ; temporary address storage
CHRPNT  .FILL 1     ; current position in input buffer
SAVY    .FILL 1     ; place to save Y register
U9F     .FILL 1

; kernal entry points
; -------------------
SETMSG  = $FF90     ; set kernel message control flag
SECOND  = $FF93     ; set secondary address after LISTEN
TKSA    = $FF96     ; send secondary address after TALK
LISTEN  = $FFB1     ; command serial bus device to LISTEN
TALK    = $FFB4     ; command serial bus device to TALK
SETLFS  = $FFBA     ; set logical file parameters
SETNAM  = $FFBD     ; set filename
ACPTR   = $FFA5     ; input byte from serial bus
CIOUT   = $FFA8     ; output byte to serial bus
UNTLK   = $FFAB     ; command serial bus device to UNTALK
UNLSN   = $FFAE     ; command serial bus device to UNLISTEN
CHKIN   = $FFC6     ; define input channel
CLRCHN  = $FFCC     ; restore default devices
INPUT   = $FFCF     ; input a character (CHRIN)
CHROUT  = $FFD2     ; output a character
LOAD    = $FFD5     ; load from device
SAVE    = $FFD8     ; save to device
STOP    = $FFE1     ; check the STOP key
GETIN   = $FFE4     ; get a character

; basic header
; ------------
.if 1
*       = $0801
        .word (+), 2005		; pointer, line number
        .null $9e, ^SUPER	; will be sys 4096
+   .word 0					; basic line end

*       = $1000
.endif

        *= $9519

; initial entry point
; -------------------
SUPER   LDY #MSG4-MSGBAS    ; display "..SYS "
        JSR SNDMSG
        LDA SUPAD           ; store entry point address in tmp0
        STA TMP0
        LDA SUPAD+1
        STA TMP0+1
        JSR CVTDEC          ; convert to decimal
        LDA #0
        LDX #6
        LDY #3
        JSR NMPRNT          ; print entry address
        JSR CRLF
        LDA LINKAD          ; set BRK vector
        STA BKVEC
        LDA LINKAD+1
        STA BKVEC+1
        LDA #$80            ; disable kernel control messages
        JSR SETMSG          ; and enable error messages
        BRK

; BRK handler
; -----------
BREAK   LDX #$05            ; pull registers off the stack
BSTACK  PLA                 ; order: Y,X,A,SR,PCL,PCH
        STA PCH,X           ; store in variables
        DEX 
        BPL BSTACK
        CLD
        TSX 
        STX SP              ; save the stack pointer
        CLI                 ; enable interupts

; display registers [R]
; ---------------------
DSPLYR  LDY #MSG2-MSGBAS    ; display headers
        JSR SNDCLR
        LDA #$3B            ; prefix registers with "; "
        JSR CHROUT
        LDA #$20
        JSR CHROUT
        LDA PCH             ; print high byte of PC
        JSR WRTWO
        LDY #1
DISJ    LDA PCH,Y           ; print rest of the registers
        JSR WRBYTE
        INY 
        CPY #7
        BCC DISJ

; get next command
; ----------------
STRT    JSR CRLF            ; new line
        LDX #0              ; point at start of input buffer
        STX CHRPNT 
SMOVE   JSR INPUT           ; get character
        STA INBUFF,X        ; store in input buffer
        INX 
        CPX #ENDIN-INBUFF   ; error if buffer is full
        BCS ERROR
        CMP #$0D            ; keep going until CR
        BNE SMOVE
        LDA #0
        STA INBUFF-1,X      ; null-terminate input buffer
ST1     JSR GETCHR
        BEQ STRT            ; start over if buffer is empty
        CMP #$20            ; skip leading spaces
        BEQ ST1
S0      LDX #KEYTOP-KEYW    ; loop through valid command characters
S1      CMP KEYW,X          ; see if character matches
        BEQ S2              ; command matched, dispatch it
        DEX                 ; no match, check next command
        BPL S1

; handle error
; ------------
ERROR   LDY #MSG3-MSGBAS    ; display "?" to indicate error
        JSR SNDMSG          
        JMP STRT            ; start over

; dispatch command
; ----------------
S2      CPX #$13            ; last 3 commands in table are load/save/validate
        BCS LSV
        CPX #$0F            ; next 4 commands are base conversions
        BCS CNVLNK
        TXA                 ; remaining commands dispatch through vector table
        ASL A               ; multiply index of command by 2 since table
        TAX                 ; contains 2-byte addresses
        LDA KADDR+1,X       ; push address from vector table onto stack
        PHA
        LDA KADDR,X
        PHA
        JMP GETPAR          ; get parameters for command
LSV     STA SAVY            ; handle load/save/validate
        JMP LD
CNVLNK  JMP CONVRT          ; handle base conversion

; exit monitor [X]
; ----------------
EXIT    JMP ($A002)         ; jump to warm-start vector to reinitialize BASIC

; display memory [M]
; ------------------
DSPLYM  BCS DSPM11          ; no parameter specified
        JSR COPY12          ; save start address
        JSR GETPAR          ; get end address
        BCC DSMNEW
DSPM11  LDA #$0B            ; show 12 lines by default
        STA TMP0
        BNE DSPBYT
DSMNEW  JSR SUB12           ; get number of bytes between address
        BCC MERROR          ; error if start is after end
        LDX #3              ; divide by 8 (shift right 3 times)
DSPM01  LSR TMP0+1
        ROR TMP0
        DEX 
        BNE DSPM01
DSPBYT  JSR STOP            ; check for stop key
        BEQ DSPMX           ; exit early if pressed
        JSR DISPMEM         ; display 1 line
        LDA #8              ; increase start address by 8 bytes
        JSR BUMPAD2
        JSR SUBA1           ; decrement line counter
        BCS DSPBYT          ; show another line until it's < 0
DSPMX   JMP STRT            ; start over
MERROR  JMP ERROR

; alter registers [;]
; -------------------
ALTR    JSR COPY1P          ; store first parameter in PC
        LDY #0              ; init counter
ALTR1   JSR GETPAR          ; get value for next paremeter
        BCS ALTRX           ; exit if no more values
        LDA TMP0            ; get parameter
        STA SR,Y            ; store in variable, offset from SR
        INY
        CPY #$05            ; less than 5?
        BCC ALTR1           ; if so, get next
ALTRX   JMP STRT            ; start over

; alter memory [>]
; ----------------
ALTM    BCS ALTMX           ; exit if no parameter
        JSR COPY12          ; copy parameter to start address
        LDY #0
ALTM1   JSR GETPAR          ; get next byte
        BCS ALTMX           ; if none, exit loop
        LDA TMP0            ; load value into A
        STA (TMP2),Y        ; save value into memory at start address + Y
        INY                 ; next byte
        CPY #8              ; have we read 8 bytes yet?
        BCC ALTM1           ; if not, loop again
ALTMX   LDA #$91            ; move cursor up
        JSR CHROUT
        JSR DISPMEM         ; re-display memory
        JMP STRT            ; start over

; goto (run) [G]
GOTO    LDX SP              ; load stack pointer from memory
        TXS                 ; save in stack pointer
GOTO2   JSR COPY1P          ; copy first parameter to PC
        SEI                 ; disable interrupts
        LDA PCH             ; push pc high byte on stack
        PHA
        LDA PCL             ; push pc low byte on stack
        PHA
        LDA SR              ; push status byte on stack
        PHA
        LDA ACC             ; load accumulator from memory
        LDX XR              ; load X from memory
        LDY YR              ; load Y from memory
        RTI                 ; return from interrupt (pops PC and SR)

; jump to subroutine [J]
JSUB    LDX SP              ; load stack pointer from memory
        TXS                 ; save value in SP register
        JSR GOTO2           ; same as goto command
        STY YR              ; save Y to memory
        STX XR              ; save X to memory
        STA ACC             ; save accumulator to memory
        PHP                 ; push processor status on stack
        PLA                 ; pull processor status into A
        STA SR              ; save processor status to memory
        JMP DSPLYR          ; display registers

; display 8 bytes of memory
; -------------------------
DISPMEM JSR CRLF            ; new line
        LDA #">"            ; prefix > so memory can be edited
        JSR CHROUT
        JSR SHOWAD          ; show address
        LDY #0
        BEQ DMEMGO          ; skip space on first byte
DMEMLP  JSR SPACE           ; print space between bytes
DMEMGO  LDA (TMP2),Y        ; second address indexed by Y
        JSR WRTWO           ; output hex for byte
        INY                 ; next byte
        CPY #8              ; have we output 8 bytes yet?
        BCC DMEMLP          ; if not, output next byte
        LDY #MSG5-MSGBAS    ; if so, output : and RVS ON
        JSR SNDMSG
        LDY #0              ; back to first byte in line
DCHAR   LDA (TMP2),Y        ; load byte
        TAX                 ; put in X
        AND #$BF            ; clear 6th bit
        CMP #$22            ; is it a quote (")
        BEQ DDOT            ; if so, print . instead
        TXA                 ; if not, restore character
        AND #$7F            ; clear top bit
        CMP #$20            ; is it a printable character (>= $20)?
        TXA                 ; restore character
        BCS DCHROK          ; if printable, output character
DDOT    LDA #$2E            ; if not, output '.' instaed
DCHROK  JSR CHROUT
        INY                 ; next byte
        CPY #8              ; have we output 8 bytes yet?
        BCC DCHAR           ; if not, output next byte
        RTS 

; compare memory [C]
; ------------------
COMPAR  LDA #0              ; 0 signals compare
        .BYTE $2C           ; absolute BIT opcode consumes next word (LDA #$80)

; transfer memory [T]
; -------------------
TRANS   LDA #$80            ; $80 signals transfer
        STA SAVY            ; save flag
        LDA #0              ; assume we're counting up (bit 7 clear)
        STA UPFLG
        JSR GETDIF          ; get two addresses and calculate difference
                            ; TMP2 = source start
                            ; STASH = source end
                            ; STORE = length
        BCS TERROR          ; carry set indicates error
        JSR GETPAR          ; get destination address in TMP0
        BCC TOKAY           ; carry set indicates error
TERROR  JMP ERROR           ; handle error
TOKAY   BIT SAVY            ; transfer or compare?
        BPL COMPAR1         ; high bit clear indicates compare
        LDA TMP2
        CMP TMP0            ; compare source start (TMP2) to destination (TMP0)
        LDA TMP2+1
        SBC TMP0+1
        BCS COMPAR1         ; count up if source is before than desitnation
        LDA STORE           ; otherwise, start at end and count down
        ADC TMP0            ; add length (STORE) to desintation (TMP0)
        STA TMP0            ; to calculate end of destination
        LDA STORE+1
        ADC TMP0+1
        STA TMP0+1
        LDX #1              ; start at end of source source
TDOWN   LDA STASH,X         ; TMP2 = source end (STASH)
        STA TMP2,X
        DEX 
        BPL TDOWN
        LDA #$80            ; count down (bit 7 set)
        STA UPFLG
COMPAR1 JSR CRLF            ; output CR
        LDY #0              ; no offset from pointer
TCLOOP  JSR STOP            ; check for stop
        BEQ TEXIT           ; exit if pressed
        LDA (TMP2),Y        ; load byte from source + Y
        BIT SAVY            ; transfer or compare?
        BPL COMPAR2         ; skip store if comparing
        STA (TMP0),Y        ; store in destination
COMPAR2 CMP (TMP0),Y        ; compare to destination
        BEQ TMVAD           ; don't show address if equal
        JSR SHOWAD          ; show address
TMVAD   BIT UPFLG           ; counting up or down?
        BMI TDECAD          ; high bit set means we're counting down
        INC TMP0            ; increment destination low byte
        BNE TINCOK
        INC TMP0+1          ; carry to high byte if necessary
        BNE TINCOK
        JMP ERROR           ; error if high byte overflowed
TDECAD  JSR SUBA1           ; decrement destination (TMP0)
        JSR SUB21           ; decrement source (TMP2)
        JMP TMOR
TINCOK  JSR ADDA2           ; increment source (TMP2)
TMOR    JSR SUB13           ; decrement length
        BCS TCLOOP          ; loop until length is 0
TEXIT   JMP STRT            ; start over

; hunt memory [H]
HUNT    JSR GETDIF
        BCS HERROR
        LDY #0
        JSR GETCHR
        CMP #"'"
        BNE NOSTRH
        JSR GETCHR
        CMP #0
        BEQ HERROR
HPAR    STA STAGE,Y
        INY 
        JSR GETCHR
        BEQ HTGO
        CPY #ESTAGE-STAGE
        BNE HPAR
        BEQ HTGO
NOSTRH  JSR RDPAR
HLP     LDA TMP0
        STA STAGE,Y
        INY 
        JSR GETPAR
        BCS HTGO
        CPY #ESTAGE-STAGE
        BNE HLP
HTGO    STY SAVY
        JSR CRLF
HSCAN   LDY #0
HLP3    LDA (TMP2),Y
        CMP STAGE,Y
        BNE HNOFT
        INY 
        CPY SAVY
        BNE HLP3
        ; MATCH FOUND
        JSR SHOWAD
HNOFT   JSR STOP
        BEQ HEXIT
        JSR ADDA2
        JSR SUB13
        BCS HSCAN
HEXIT   JMP STRT
HERROR  JMP ERROR

; load, save, or verify [LSV]
LD      LDY #1
        STY FA
        STY SADD
        DEY
        STY FNLEN
        STY SATUS
        LDA #>STAGE
        STA FNADR+1
        LDA #<STAGE
        STA FNADR
L1      JSR GETCHR
        BEQ LSHORT
        CMP #$20
        BEQ L1
        CMP #$22
        BNE LERROR
        LDX CHRPNT
L3      LDA INBUFF,X
        BEQ LSHORT
        INX 
        CMP #$22
        BEQ L8
        STA (FNADR),Y
        INC FNLEN
        INY 
        CPY #ESTAGE-STAGE
        BCC L3
LERROR  JMP ERROR
L8      STX CHRPNT
        JSR GETCHR
        BEQ LSHORT
        JSR GETPAR
        BCS LSHORT
        LDA TMP0
        STA FA
        JSR GETPAR
        BCS LSHORT
        JSR COPY12
        JSR GETPAR
        BCS LDADDR
        JSR CRLF
        LDX TMP0
        LDY TMP0+1
        LDA SAVY
        CMP #"S"
        BNE LERROR
        LDA #0
        STA SADD
        LDA #TMP2
        JSR SAVE
LSVXIT  JMP STRT
LSHORT  LDA SAVY
        CMP #"V"
        BEQ LOADIT
        CMP #"L"
        BNE LERROR
        LDA #0
LOADIT  JSR LOAD
        LDA SATUS
        AND #$10
        BEQ LSVXIT
        LDA SAVY
        BEQ LERROR
        LDY #MSG6-MSGBAS
        JSR SNDMSG
        JMP STRT
LDADDR  LDX TMP2
        LDY TMP2+1
        LDA #0
        STA SADD
        BEQ LSHORT

; fill memory [F]
FILL    JSR GETDIF
        BCS AERROR
        JSR GETPAR
        BCS AERROR
        JSR GETCHR
        BNE AERROR
        LDY #0
FILLP   LDA TMP0
        STA (TMP2),Y
        JSR STOP
        BEQ FSTART
        JSR ADDA2
        JSR SUB13
        BCS FILLP
FSTART  JMP STRT

; assemble [A.]
ASSEM   BCS AERROR
        JSR COPY12
AGET1   LDX #0
        STX U0AA0+1
        STX DIGCNT
AGET2   JSR GETCHR
        BNE ALMOR
        CPX #0
        BEQ FSTART
ALMOR   CMP #$20
        BEQ AGET1
        STA MNEMW,X
        INX
        CPX #3
        BNE AGET2
ASQEEZ  DEX 
        BMI AOPRND
        LDA MNEMW,X
        SEC
        SBC #$3F
        LDY #$05
ASHIFT  LSR A
        ROR U0AA0+1
        ROR U0AA0
        DEY
        BNE ASHIFT
        BEQ ASQEEZ
AERROR  JMP ERROR
        ; GET THE OPERAND
AOPRND  LDX #2
ASCAN   LDA DIGCNT
        BNE AFORM1
        ; LOOK FOR MODE CHARS
        JSR RDVAL
        BEQ AFORM0
        BCS AERROR
        LDA #"$"
        STA U0AA0,X
        INX
        LDY #4
        LDA NUMBIT
        CMP #8
        BCC AADDR
        CPY DIGCNT
        BEQ AFILL0
AADDR   LDA TMP0+1
        BNE AFILL0
        LDY #2               ;ZERO PGE MODE
AFILL0  LDA #$30
AFIL0L  STA U0AA0,X
        INX
        DEY
        BNE AFIL0L
        ; GET FORMAT CHAR
AFORM0  DEC CHRPNT
AFORM1  JSR GETCHR
        BEQ AESCAN
        CMP #$20
        BEQ ASCAN
        STA U0AA0,X
        INX
        CPX #U0AAE-U0AA0
        BCC ASCAN
        BCS AERROR

AESCAN  STX STORE
        LDX #0
        STX OPCODE

ATRYOP  LDX #0
        STX U9F
        LDA OPCODE
        JSR INSTXX
        LDX ACMD
        STX STORE+1
        TAX
        LDA MNEMR,X
        JSR CHEKOP
        LDA MNEML,X
        JSR CHEKOP
        LDX #6
TRYIT   CPX #3
        BNE TRYMOD
        LDY LENGTH
        BEQ TRYMOD
TRYAD   LDA ACMD
        CMP #$E8
        LDA #$30
        BCS TRY4B
        JSR CHEK2B
        DEY
        BNE TRYAD
TRYMOD  ASL ACMD
        BCC UB4DF
        LDA CHAR1-1,X
        JSR CHEKOP
        LDA CHAR2-1,X
        BEQ UB4DF
        JSR CHEKOP
UB4DF   DEX
        BNE TRYIT
        BEQ TRYBRAN

TRY4B   JSR CHEK2B
        JSR CHEK2B
TRYBRAN LDA STORE
        CMP U9F
        BEQ ABRAN
        JMP BUMPOP
        ; CHECK BRANCH
ABRAN   LDY LENGTH
        BEQ A1BYTE
        LDA STORE+1
        CMP #$9D
        BNE OBJPUT
        LDA TMP0
        SBC TMP2
        TAX
        LDA TMP0+1
        SBC TMP2+1
        BCC ABBACK
        BNE SERROR
        CPX #$82
        BCS SERROR
        BCC ABRANX
ABBACK  TAY
        INY
        BNE SERROR
        CPX #$82
        BCC SERROR
ABRANX  DEX
        DEX
        TXA
        LDY LENGTH
        BNE OBJP2
OBJPUT  LDA TMP0-1,Y
OBJP2   STA (TMP2),Y
        DEY
        BNE OBJPUT
A1BYTE  LDA OPCODE
        STA (TMP2),Y
        JSR CRLF
        LDA #$91
        JSR CHROUT
        LDY #MSG7-MSGBAS
        JSR SNDCLR
        JSR DISLIN
        INC LENGTH
        LDA LENGTH
        JSR BUMPAD2
        ; STUFF KEYBOARD BUFFER
        LDA #"A"
        STA KEYD
        LDA #" "
        STA KEYD+1
        STA KEYD+6
        LDA TMP2+1
        JSR ASCTWO
        STA KEYD+2
        STX KEYD+3
        LDA TMP2
        JSR ASCTWO
        STA KEYD+4
        STX KEYD+5
        LDA #7
        STA NDX
        JMP STRT
SERROR  JMP ERROR

CHEK2B  JSR CHEKOP

CHEKOP  STX SAVX
        LDX U9F
        CMP U0AA0,X
        BEQ OPOK
        PLA
        PLA

BUMPOP  INC OPCODE
        BEQ SERROR
        JMP ATRYOP
OPOK    INC U9F
        LDX SAVX
        RTS

; disassemble [D]
DISASS  BCS DIS0AD
        JSR COPY12
        JSR GETPAR
        BCC DIS2AD
DIS0AD  LDA #$14
        STA TMP0
        BNE DISGO
DIS2AD  JSR SUB12
        BCC DERROR
DISGO   JSR CLINE
        JSR STOP
        BEQ DISEXIT
        JSR DSOUT1
        INC LENGTH
        LDA LENGTH
        JSR BUMPAD2
        LDA LENGTH
        JSR SUBA2
        BCS DISGO
DISEXIT JMP STRT
DERROR  JMP ERROR

DSOUT1  LDA #"."
        JSR CHROUT
        JSR SPACE

DISLIN  JSR SHOWAD
        JSR SPACE
        LDY #0
        LDA (TMP2),Y
        JSR INSTXX
        PHA
        LDX LENGTH
        INX
DSBYT   DEX 
        BPL DSHEX
        STY SAVY
        LDY #MSG8-MSGBAS
        JSR SNDMSG
        LDY SAVY
        JMP NXBYT
DSHEX   LDA (TMP2),Y
        JSR WRBYTE

NXBYT   INY 
        CPY #3
        BCC DSBYT
        PLA
        LDX #3
        JSR PROPXX
        LDX #6
PRADR1  CPX #3
        BNE PRADR3
        LDY LENGTH
        BEQ PRADR3
PRADR2  LDA ACMD
        CMP #$E8
        PHP
        LDA (TMP2),Y
        PLP
        BCS RELAD
        JSR WRTWO
        DEY
        BNE PRADR2
PRADR3  ASL ACMD
        BCC PRADR4
        LDA CHAR1-1,X
        JSR CHROUT
        LDA CHAR2-1,X
        BEQ PRADR4
        JSR CHROUT
PRADR4  DEX 
        BNE PRADR1
        RTS
RELAD   JSR UB64D
        CLC
        ADC #1
        BNE RELEND
        INX
RELEND  JMP WRADDR

UB64D   LDX TMP2+1
        TAY
        BPL RELC2
        DEX
RELC2   ADC TMP2
        BCC RELC3
        INX
RELC3   RTS 
        ; GET OPCODE MODE,LEN
INSTXX  TAY 
        LSR A
        BCC IEVEN
        LSR A
        BCS ERR
        CMP #$22
        BEQ ERR  ;KILL $89
        AND #$07
        ORA #$80
IEVEN   LSR A
        TAX
        LDA MODE,X
        BCS RTMODE
        LSR A
        LSR A
        LSR A
        LSR A
RTMODE  AND #$0F
        BNE GETFMT
ERR     LDY #$80
        LDA #0
GETFMT  TAX
        LDA MODE2,X
        STA ACMD
        AND #$03
        STA LENGTH
        TYA
        AND #$8F
        TAX
        TYA
        LDY #3
        CPX #$8A
        BEQ GTFM4
GTFM2   LSR A
        BCC GTFM4
        LSR A
GTFM3   LSR A
        ORA #$20
        DEY
        BNE GTFM3
        INY
GTFM4   DEY
        BNE GTFM2
        RTS

PROPXX  TAY 
        LDA MNEML,Y
        STA STORE
        LDA MNEMR,Y
        STA STORE+1
PRMN1   LDA #0
        LDY #$05
PRMN2   ASL STORE+1
        ROL STORE
        ROL A
        DEY
        BNE PRMN2
        ADC #$3F
        JSR CHROUT
        DEX
        BNE PRMN1
        JMP SPACE

; read parameters
; ---------------
RDPAR   DEC CHRPNT      ; back up one char
GETPAR  JSR RDVAL       ; read the value
        BCS GTERR       ; carry set indicates error
        JSR GOTCHR      ; check previous character
        BNE CKTERM      ; if it's not null, check if it's a valid separator
        DEC CHRPNT      ; back up one char
        LDA DIGCNT      ; get number of digits read
        BNE GETGOT      ; found some digits
        BEQ GTNIL       ; didn't find any digits
CKTERM  CMP #$20        ; space or comma are valid separators
        BEQ GETGOT      ; anything else is an error
        CMP #","
        BEQ GETGOT
GTERR   PLA             ; encountered error
        PLA             ; get rid of command vector pushed on stack
        JMP ERROR       ; handle error
GTNIL   SEC             ; set carry to indicate no parameter found
        .BYTE $24       ; BIT ZP opcode consumes next byte (CLC)
GETGOT  CLC             ; clear carry to indicate paremeter returned
        LDA DIGCNT      ; return number of digits in A
        RTS             ; return to address pushed from vector table

; read a value in the specified base
; ----------------------------------
RDVAL   LDA #0          ; clear temp
        STA TMP0
        STA TMP0+1
        STA DIGCNT      ; clear digit counter
        TXA             ; save X and Y
        PHA
        TYA
        PHA
RDVMOR  JSR GETCHR      ; get next character from input buffer
        BEQ RDNILK      ; null at end of buffer
        CMP #$20        ; skip spaces
        BEQ RDVMOR
        LDX #3          ; check numeric base [$+&%]
GNMODE  CMP HIKEY,X
        BEQ GOTMOD      ; got a match, set it up
        DEX
        BPL GNMODE      ; check next base
        INX             ; default to decimal
        DEC CHRPNT      ; back up one character
GOTMOD  LDY MODTAB,X    ; get base
        LDA LENTAB,X    ; get bits per digit
        STA NUMBIT      ; store bits per digit 
NUDIG   JSR GETCHR      ; get next char in A
RDNILK  BEQ RDNIL       ; end of number if no more characters
        SEC
        SBC #$30        ; subtract ascii value of 0 to get numeric value
        BCC RDNIL       ; end of number if character was less than 0
        CMP #$0A
        BCC DIGMOR      ; not a hex digit if less than A
        SBC #$07        ; 7 chars between ascii 9 and A, so subtract 7
        CMP #$10        ; end of number if char is greater than F
        BCS RDNIL
DIGMOR  STA INDIG       ; store the digit
        CPY INDIG       ; compare base with the digit
        BCC RDERR       ; error if the digit >= the base
        BEQ RDERR
        INC DIGCNT      ; increment the number of digits
        CPY #10
        BNE NODECM      ; skip the next part if not using base 10
        LDX #1
DECLP1  LDA TMP0,X      ; stash the previous 16-bit value for later use
        STA STASH,X
        DEX
        BPL DECLP1
NODECM  LDX NUMBIT      ; number of bits to shift
TIMES2  ASL TMP0        ; shift 16-bit value by specified number of bits
        ROL TMP0+1
        BCS RDERR       ; error if we overflowed 16 bits
        DEX
        BNE TIMES2      ; shift remaining bits
        CPY #10
        BNE NODEC2      ; skip the next part if not using base 10
        ASL STASH       ; shift the previous 16-bit value one bit left
        ROL STASH+1
        BCS RDERR       ; error if we overflowed 16 bits
        LDA STASH       ; add shifted previous value to current value
        ADC TMP0
        STA TMP0
        LDA STASH+1
        ADC TMP0+1
        STA TMP0+1
        BCS RDERR       ; error if we overflowed 16 bits
NODEC2  CLC 
        LDA INDIG       ; load current digit
        ADC TMP0        ; add current digit to low byte
        STA TMP0        ; and store result in low byte
        TXA             ; A=0
        ADC TMP0+1      ; add carry to high byte
        STA TMP0+1      ; and store result in high byte
        BCC NUDIG       ; get next digit if we didn't overflow
RDERR   SEC             ; set carry to indicate error
        .BYTE $24       ; BIT ZP opcode consumes next byte (CLC)
RDNIL   CLC             ; clear carry to indicate success
        STY NUMBIT      ; save number of bits
        PLA             ; restore X and Y
        TAY
        PLA
        TAX
        LDA DIGCNT      ; return number of digits in A
        RTS

; print address
; -------------
SHOWAD  LDA TMP2
        LDX TMP2+1

WRADDR  PHA             ; save low byte
        TXA             ; put high byte in A
        JSR WRTWO       ; output high byte
        PLA             ; restore low byte

WRBYTE  JSR WRTWO       ; output byte in A

SPACE   LDA #$20        ; output space
        BNE FLIP

CHOUT   CMP #$0D        ; output char with special handling of CR
        BNE FLIP
CRLF    LDA #$0D        ; load CR in A
        BIT $13         ; check default channel
        BPL FLIP        ; if high bit is clear output CR only
        JSR CHROUT      ; otherwise output CR+LF
        LDA #$0A        ; output LF
FLIP    JMP CHROUT

FRESH   JSR CRLF        ; output CR
        LDA #$20        ; load space in A
        JSR CHROUT
        JMP SNCLR

; output two hex digits for byte
; ------------------------------
WRTWO   STX SAVX        ; save X
        JSR ASCTWO      ; get hex chars for byte in X (lower) and A (upper)
        JSR CHROUT      ; output upper nybble
        TXA             ; transfer lower to A
        LDX SAVX        ; restore X
        JMP CHROUT      ; output lower nybble

; convert byte in A to hex digits
; -------------------------------
ASCTWO  PHA             ; save byte
        JSR ASCII       ; do low nybble
        TAX             ; save in X
        PLA             ; restore byte
        LSR A           ; shift upper nybble down
        LSR A
        LSR A
        LSR A

; convert low nibble in A to hex digit
; ------------------------------------
ASCII   AND #$0F        ; clear upper nibble
        CMP #$0A        ; if less than A, skip next step
        BCC ASC1
        ADC #6          ; skip ascii chars between 9 and A
ASC1    ADC #$30        ; add ascii char 0 to value
        RTS

; get prev char from input buffer
; -------------------------------
GOTCHR  DEC CHRPNT

; get next char from input buffer
; -------------------------------
GETCHR  STX SAVX
        LDX CHRPNT      ; get pointer to next char
        LDA INBUFF,X    ; load next char in A
        BEQ NOCHAR      ; null, :, or ? signal end of buffer
        CMP #":"        
        BEQ NOCHAR
        CMP #"?"
NOCHAR  PHP
        INC CHRPNT      ; next char
        LDX SAVX
        PLP             ; Z flag will signal last character
        RTS

; copy TMP0 to TMP2
; -----------------
COPY12  LDA TMP0        ; low byte
        STA TMP2
        LDA TMP0+1      ; high byte
        STA TMP2+1
        RTS

; subtract TMP2 from TMP0
; -----------------------
SUB12   SEC
        LDA TMP0        ; subtract low byte
        SBC TMP2
        STA TMP0
        LDA TMP0+1
        SBC TMP2+1      ; subtract high byte
        STA TMP0+1
        RTS


; subtract from TMP0
; ------------------
SUBA1   LDA #1          ; shortcut to decrement by 1
SUBA2   STA SAVX        ; subtrahend in accumulator
        SEC
        LDA TMP0        ; minuend in low byte
        SBC SAVX
        STA TMP0
        LDA TMP0+1      ; borrow from high byte
        SBC #0
        STA TMP0+1
        RTS

; subtract 1 from STORE
; ---------------------
SUB13   SEC
        LDA STORE
        SBC #1          ; decrement low byte
        STA STORE
        LDA STORE+1
        SBC #0          ; borrow from high byte
        STA STORE+1
        RTS

; add to TMP2
; -----------
ADDA2   LDA #1          ; shortcut to increment by 1
BUMPAD2 CLC
        ADC TMP2        ; add value in accumulator to low byte
        STA TMP2
        BCC BUMPEX
        INC TMP2+1      ; carry to high byte
BUMPEX  RTS 

; subtract 1 from TMP2
; --------------------
SUB21   SEC
        LDA TMP2        ; decrement low byte
        SBC #1
        STA TMP2
        LDA TMP2+1      ; borrow from high byte
        SBC #0
        STA TMP2+1
        RTS

; copy TMP0 to PC
; ---------------
COPY1P  BCS CPY1PX      ; do nothing if parameter is empty
        LDA TMP0        ; copy low byte
        LDY TMP0+1      ; copy high byte
        STA PCL
        STY PCH
CPY1PX  RTS 

; get start/end addresses and calc difference
; -------------------------------------------
GETDIF  BCS GDIFX       ; exit with error if no parameter given
        JSR COPY12      ; save start address in TMP2
        JSR GETPAR      ; get end address in TMP0
        BCS GDIFX       ; exit with error if no parameter given
        LDA TMP0        ; save end address in STASH
        STA STASH
        LDA TMP0+1
        STA STASH+1
        JSR SUB12       ; subtract start address from end address
        LDA TMP0
        STA STORE       ; save difference in STORE
        LDA TMP0+1
        STA STORE+1
        BCC GDIFX       ; error if start address is after end address
        CLC             ; clear carry to indicate success
        .BYTE $24       ; BIT ZP opcode consumes next byte (SEC)
GDIFX   SEC             ; set carry to indicate error
        RTS

; convert base [$+&%]
; -------------------
CONVRT  JSR RDPAR
        JSR FRESH
        LDA #"$"
        JSR CHROUT
        LDA TMP0
        LDX TMP0+1
        JSR WRADDR
        JSR FRESH
        LDA #"+"
        JSR CHROUT
        JSR CVTDEC
        LDA #0
        LDX #6
        LDY #3
        JSR NMPRNT
        JSR FRESH
        LDA #"&"
        JSR CHROUT
        LDA #0
        LDX #8
        LDY #2
        JSR PRINUM
        JSR FRESH
        LDA #"%"
        JSR CHROUT
        LDA #0
        LDX #$18
        LDY #0
        JSR PRINUM
        JMP STRT

CVTDEC  JSR COPY12
        LDA #0
        LDX #2
DECML1  STA U0AA0,X
        DEX
        BPL DECML1
        ; CONVERT TO DECIMAL
        LDY #16
        PHP
        SEI
        SED
DECML2  ASL TMP2
        ROL TMP2+1
        LDX #2
DECDBL  LDA U0AA0,X
        ADC U0AA0,X
        STA U0AA0,X
        DEX
        BPL DECDBL
        DEY
        BNE DECML2
        PLP
        RTS

PRINUM  PHA 
        LDA TMP0
        STA U0AA0+2
        LDA TMP0+1
        STA U0AA0+1
        LDA #0
        STA U0AA0
        PLA

; PRINT WITH ZERO SUPPR
NMPRNT  STA DIGCNT
        STY NUMBIT
DIGOUT  LDY NUMBIT
        LDA #0
ROLBIT  ASL U0AA0+2
        ROL U0AA0+1
        ROL U0AA0
        ROL A
        DEY
        BPL ROLBIT
        TAY
        BNE NZERO
        CPX #1
        BEQ NZERO
        LDY DIGCNT
        BEQ ZERSUP
NZERO   INC DIGCNT
        ORA #$30
        JSR CHROUT
ZERSUP  DEX 
        BNE DIGOUT
        RTS

; disk status/command [@]
DSTAT   BNE CHGDEV
        LDX #8
        .BYTE $2C           ; absolute BIT opcode consumes next word (LDX TMP0)
CHGDEV  LDX TMP0
        CPX #4
        BCC IOERR
        CPX #32
        BCS IOERR
        STX TMP0
        LDA #0
        STA SATUS
        STA FNLEN
        JSR GETCHR
        BEQ INSTAT1
        DEC CHRPNT
        CMP #"$"
        BEQ DIRECT
        LDA TMP0
        JSR LISTEN
        LDA #$6F
        JSR SECOND
DCOMD   LDX CHRPNT
        INC CHRPNT
        LDA INBUFF,X
        BEQ INSTAT
        JSR CIOUT
        BCC DCOMD
INSTAT  JSR UNLSN
INSTAT1 JSR CRLF
        LDA TMP0
        JSR TALK
        LDA #$6F
        JSR TKSA
RDSTAT  JSR ACPTR
        JSR CHROUT
        CMP #$0D
        BEQ DEXIT
        LDA SATUS
        AND #$BF
        BEQ RDSTAT
DEXIT   JSR UNTLK
        JMP STRT
IOERR   JMP ERROR
DIRECT  LDA TMP0
        JSR LISTEN
        LDA #$F0
        JSR SECOND
        LDX CHRPNT
DIR2    LDA INBUFF,X
        BEQ DIR3
        JSR CIOUT
        INX
        BNE DIR2
DIR3    JSR UNLSN
        JSR CRLF
        LDA TMP0
        PHA
        JSR TALK
        LDA #$60
        JSR TKSA
        LDY #3
DIRLIN  STY STORE
DLINK   JSR ACPTR
        STA TMP0
        LDA SATUS
        BNE DREXIT
        JSR ACPTR
        STA TMP0+1
        LDA SATUS
        BNE DREXIT
        DEC STORE
        BNE DLINK
        JSR CVTDEC
        LDA #0
        LDX #6
        LDY #3
        JSR NMPRNT
        LDA #" "
        JSR CHROUT
DNAME   JSR ACPTR
        BEQ DMORE
        LDX SATUS
        BNE DREXIT
        JSR CHROUT
        CLC
        BCC DNAME
DMORE   JSR CRLF
        JSR STOP
        BEQ DREXIT
        JSR GETIN
        BEQ NOPAWS
PAWS    JSR GETIN
        BEQ PAWS
NOPAWS  LDY #2
        BNE DIRLIN
DREXIT  JSR UNTLK
        PLA
        JSR LISTEN
        LDA #$E0
        JSR SECOND
        JSR UNLSN
        JMP STRT

; print and clear routines
; ------------------------
CLINE   JSR CRLF                ; send CR+LF
        JMP SNCLR               ; clear line
SNDCLR  JSR SNDMSG
SNCLR   LDY #$28                ; loop 40 times
SNCLP   LDA #$20                ; output space character
        JSR CHROUT
        LDA #$14                ; output delete character
        JSR CHROUT
        DEY
        BNE SNCLP
        RTS

; display message from table
; --------------------------
SNDMSG  LDA MSGBAS,Y            ; Y contains offset in msg table
        PHP
        AND #$7F                ; strip high bit before output
        JSR CHOUT
        INY
        PLP
        BPL SNDMSG              ; loop until high bit is set
        RTS

; message table, last character has high bit set
; ----------------------------------------------
MSGBAS  =*
MSG2    .BYTE $0D               ; header for registers
        .TEXT "   PC  SR AC XR YR SP   V1.2"
        .BYTE $0D+$80
MSG3    .BYTE $1D,$3F+$80       ; syntax error: move right, display "?"
MSG4    .TEXT "..SYS"           ; SYS call to enter monitor
        .BYTE $20+$80
MSG5    .BYTE $3A,$12+$80       ; ":" then RVS ON for memory ASCII dump
MSG6    .TEXT " ERRO"           ; I/O error: display " ERROR"
        .BYTE "R"+$80
MSG7    .BYTE $41,$20+$80       ; assemble next instruction: "A " + addr
MSG8    .TEXT "  "              ; pad non-existent byte: skip 3 spaces
        .BYTE $20+$80

; MODE TABLE... NYBBLE ORGANIZED
; 0=ERR  4=IMPLIED  8=ZER,X  C=ZER,Y
; 1=IMM  5=ACC      8=ABS,X  D=REL
; 2=ZER  6=(IND,X)  A=ABS,Y
; 3=ABS  7=(IND),Y  B=(IND)
MODE    .BYTE $40,$02,$45,$03
        .BYTE $D0,$08,$40,$09
        .BYTE $30,$22,$45,$33
        .BYTE $D0,$08,$40,$09
        .BYTE $40,$02,$45,$33
        .BYTE $D0,$08,$40,$09
        .BYTE $40,$02,$45,$B3
        .BYTE $D0,$08,$40,$09
        .BYTE $00,$22,$44,$33
        .BYTE $D0,$8C,$44,$00
        .BYTE $11,$22,$44,$33
        .BYTE $D0,$8C,$44,$9A
        .BYTE $10,$22,$44,$33
        .BYTE $D0,$08,$40,$09
        .BYTE $10,$22,$44,$33
        .BYTE $D0,$08,$40,$09
        .BYTE $62,$13,$78,$A9

MODE2   .BYTE $00,$21,$81,$82
        .BYTE $00,$00,$59,$4D
        .BYTE $91,$92,$86,$4A
        .BYTE $85,$9D

CHAR1   .BYTE $2C,$29,$2C
        .BYTE $23,$28,$24

CHAR2   .BYTE $59,$00,$58
        .BYTE $24,$24,$00

MNEML   .BYTE $1C,$8A,$1C,$23
        .BYTE $5D,$8B,$1B,$A1
        .BYTE $9D,$8A,$1D,$23
        .BYTE $9D,$8B,$1D,$A1
        .BYTE $00,$29,$19,$AE
        .BYTE $69,$A8,$19,$23
        .BYTE $24,$53,$1B,$23
        .BYTE $24,$53,$19,$A1
        .BYTE $00,$1A,$5B,$5B
        .BYTE $A5,$69,$24,$24
        .BYTE $AE,$AE,$A8,$AD
        .BYTE $29,$00,$7C,$00
        .BYTE $15,$9C,$6D,$9C
        .BYTE $A5,$69,$29,$53
        .BYTE $84,$13,$34,$11
        .BYTE $A5,$69,$23,$A0
MNEMR   .BYTE $D8,$62,$5A,$48
        .BYTE $26,$62,$94,$88
        .BYTE $54,$44,$C8,$54
        .BYTE $68,$44,$E8,$94
        .BYTE $00,$B4,$08,$84
        .BYTE $74,$B4,$28,$6E
        .BYTE $74,$F4,$CC,$4A
        .BYTE $72,$F2,$A4,$8A
        .BYTE $00,$AA,$A2,$A2
        .BYTE $74,$74,$74,$72
        .BYTE $44,$68,$B2,$32
        .BYTE $B2,$00,$22,$00
        .BYTE $1A,$1A,$26,$26
        .BYTE $72,$72,$88,$C8
        .BYTE $C4,$CA,$26,$48
        .BYTE $44,$44,$A2,$C8
        .BYTE $0D,$20,$20,$20

; single-character commands
; -------------------------
KEYW    .TEXT "ACDFGHJMRTX@.>;"
HIKEY   .TEXT "$+&%LSV"
KEYTOP  =*

; command vectors
; ---------------
KADDR   .WORD ASSEM-1,COMPAR-1,DISASS-1,FILL-1
        .WORD GOTO-1,HUNT-1,JSUB-1,DSPLYM-1
        .WORD DSPLYR-1,TRANS-1,EXIT-1,DSTAT-1
        .WORD ASSEM-1,ALTM-1,ALTR-1

MODTAB  .BYTE $10,$0A,$08,02    ; modulo number systems
LENTAB  .BYTE $04,$03,$03,$01   ; bits per digit

LINKAD  .WORD BREAK             ; address of brk handler
SUPAD   .WORD SUPER             ; address of entry point
