; ********************************
; * SUPERMON+ 64 JIM BUTTERFIELD *
; * V1.2   AUGUST 20 1985        *
; ********************************

; Reformatted and annotated in late 2016/early 2017 by J.B. Langston.
; 
; I've made the minimum necessary changes to this code to get it to assemble
; with 64tass.  Specifically, I changed the following directives from PAL
; that 64tass doesn't support:
;   - .ASC => .TEXT
;   - *=*+X => .FILL X
;
; Aside from this, I have adopted a strict whitespace and comments only
; policy so that I preserve code exactly as Jim Butterfield wrote it.
; 
; I think my comments are correct but I don't guarantee I haven't made
; any errors. Sadly Jim isn't around to ask anymore. If you spot any
; misunderstanings or errors in my comments, please report them.

; -----------------------------------------------------------------------------
; temporary pointers
TMP0    = $C1               ; used to return input, often holds end address
TMP2    = $C3               ; usually holds start address

; -----------------------------------------------------------------------------
; kernal variables
SATUS   = $90               ; kernal i/o status word
FNLEN   = $B7               ; length of current filename
SADD    = $B9               ; current secondary address (official name SA)
FA      = $BA               ; current device number
FNADR   = $BB               ; pointer to current filename
NDX     = $C6               ; number of characters in keyboard buffer
KEYD    = $0277             ; keyboard buffer
BKVEC   = $0316             ; BRK instruction vector (official name CBINV)

        *= $0100            ; store variables in tape error buffer

; -----------------------------------------------------------------------------
; variables
ACMD    .FILL 1             ; addressing command
LENGTH  .FILL 1             ; length of operand
MNEMW   .FILL 3             ; 3 letter mnemonic buffer
SAVX    .FILL 1             ; 1 byte temp storage, often to save X register
OPCODE  .FILL 1             ; current opcode for assembler/disassembler
UPFLG   .FILL 1             ; flag: count up (bit 7 clear) or down (bit 7 set)
DIGCNT  .FILL 1             ; digit count
INDIG   .FILL 1             ; numeric value of single digit
NUMBIT  .FILL 1             ; numeric base of input
STASH   .FILL 2             ; 2-byte temp storage
U0AA0   .FILL 10            ; work buffer
U0AAE   =*                  ; end of work buffer
STAGE   .FILL 30            ; staging buffer for filename, search, etc.
ESTAGE  =*                  ; end of staging buffer

        *= $0200            ; store more variables in basic line editor buffer

INBUFF  .FILL 40            ; 40-character input buffer
ENDIN   =*                  ; end of input buffer

; the next 7 locations are used to store the registers when
; entering the monitor and restore them when exiting.

PCH     .FILL 1             ; program counter high byte
PCL     .FILL 1             ; program counter low byte
SR      .FILL 1             ; status register
ACC     .FILL 1             ; accumulator
XR      .FILL 1             ; X register
YR      .FILL 1             ; Y register
SP      .FILL 1             ; stack pointer

STORE   .FILL 2             ; 2-byte temp storage
CHRPNT  .FILL 1             ; current position in input buffer
SAVY    .FILL 1             ; temp storage, often to save Y register
U9F     .FILL 1             ; index into assembler work buffer

; -----------------------------------------------------------------------------
; kernal entry points
SETMSG  = $FF90             ; set kernel message control flag
SECOND  = $FF93             ; set secondary address after LISTEN
TKSA    = $FF96             ; send secondary address after TALK
LISTEN  = $FFB1             ; command serial bus device to LISTEN
TALK    = $FFB4             ; command serial bus device to TALK
SETLFS  = $FFBA             ; set logical file parameters
SETNAM  = $FFBD             ; set filename
ACPTR   = $FFA5             ; input byte from serial bus
CIOUT   = $FFA8             ; output byte to serial bus
UNTLK   = $FFAB             ; command serial bus device to UNTALK
UNLSN   = $FFAE             ; command serial bus device to UNLISTEN
CHKIN   = $FFC6             ; define input channel
CLRCHN  = $FFCC             ; restore default devices
INPUT   = $FFCF             ; input a character (official name CHRIN)
CHROUT  = $FFD2             ; output a character
LOAD    = $FFD5             ; load from device
SAVE    = $FFD8             ; save to device
STOP    = $FFE1             ; check the STOP key
GETIN   = $FFE4             ; get a character

; -----------------------------------------------------------------------------
; set up origin

        .WEAK
ORG     = $9519
        .ENDWEAK

*       = ORG

; -----------------------------------------------------------------------------
; initial entry point
SUPER   LDY #MSG4-MSGBAS    ; display "..SYS "
        JSR SNDMSG
        LDA SUPAD           ; store entry point address in tmp0
        STA TMP0
        LDA SUPAD+1
        STA TMP0+1
        JSR CVTDEC          ; convert address to decimal
        LDA #0
        LDX #6
        LDY #3
        JSR NMPRNT          ; print entry point address
        JSR CRLF
        LDA LINKAD          ; set BRK vector
        STA BKVEC
        LDA LINKAD+1
        STA BKVEC+1
        LDA #$80            ; disable kernel control messages
        JSR SETMSG          ; and enable error messages
        BRK

; -----------------------------------------------------------------------------
; BRK handler
BREAK   LDX #$05            ; pull registers off the stack
BSTACK  PLA                 ; order: Y,X,A,SR,PCL,PCH
        STA PCH,X           ; store in memory
        DEX 
        BPL BSTACK
        CLD                 ; disable bcd mode
        TSX                 ; store stack pointer in memory 
        STX SP
        CLI                 ; enable interupts

; -----------------------------------------------------------------------------
; display registers [R]
DSPLYR  LDY #MSG2-MSGBAS    ; display headers
        JSR SNDCLR
        LDA #$3B            ; prefix registers with "; " to allow editing
        JSR CHROUT
        LDA #$20
        JSR CHROUT
        LDA PCH             ; print 2-byte program counter
        JSR WRTWO
        LDY #1              ; start 1 byte after PC high byte
DISJ    LDA PCH,Y           ; loop through rest of the registers
        JSR WRBYTE          ; print 1-byte register value
        INY 
        CPY #7              ; there are a total of 5 registers to print
        BCC DISJ

; -----------------------------------------------------------------------------
; main loop
STRT    JSR CRLF            ; new line
        LDX #0              ; point at start of input buffer
        STX CHRPNT 
SMOVE   JSR INPUT           ; CHRIN kernal call to input a character
        STA INBUFF,X        ; store in input buffer
        INX 
        CPX #ENDIN-INBUFF   ; error if buffer is full
        BCS ERROR
        CMP #$0D            ; keep reading until CR
        BNE SMOVE
        LDA #0              ; null-terminate input buffer
        STA INBUFF-1,X      ; (replacing the CR)
ST1     JSR GETCHR          ; get a character from the buffer
        BEQ STRT            ; start over if buffer is empty
        CMP #$20            ; skip leading spaces
        BEQ ST1
S0      LDX #KEYTOP-KEYW    ; loop through valid command characters
S1      CMP KEYW,X          ; see if input character matches
        BEQ S2              ; command matched, dispatch it
        DEX                 ; no match, check next command
        BPL S1              ; keep trying until we've checked them all
                            ; then fall through to error handler

; -----------------------------------------------------------------------------
; handle error
ERROR   LDY #MSG3-MSGBAS    ; display "?" to indicate error and go to new line
        JSR SNDMSG
        JMP STRT            ; back to main loop

; -----------------------------------------------------------------------------
; dispatch command
S2      CPX #$13            ; last 3 commands in table are load/save/validate
        BCS LSV             ;   which are handled by the same subroutine
        CPX #$0F            ; next 4 commands are base conversions
        BCS CNVLNK          ;   which are handled by the same subroutine
        TXA                 ; remaining commands dispatch through vector table
        ASL A               ; multiply index of command by 2
        TAX                 ;   since table contains 2-byte addresses
        LDA KADDR+1,X       ; push address from vector table onto stack
        PHA                 ;   so that the RTS from GETPAR will jump there
        LDA KADDR,X
        PHA
        JMP GETPAR          ; get the first parameter for the command
LSV     STA SAVY            ; handle load/save/validate
        JMP LD
CNVLNK  JMP CONVRT          ; handle base conversion

; -----------------------------------------------------------------------------
; exit monitor [X]
EXIT    JMP ($A002)         ; jump to warm-start vector to reinitialize BASIC

; -----------------------------------------------------------------------------
; display memory [M]
DSPLYM  BCS DSPM11          ; start from previous end addr if no address given
        JSR COPY12          ; save start address in TMP2
        JSR GETPAR          ; get end address in TMP0
        BCC DSMNEW          ; did user specify one?
DSPM11  LDA #$0B            ; if not, show 12 lines by default
        STA TMP0
        BNE DSPBYT          ; always true, but BNE uses 1 byte less than JMP
DSMNEW  JSR SUB12           ; end addr given, calc bytes between start and end
        BCC MERROR          ; error if start is after end
        LDX #3              ; divide by 8 (shift right 3 times)
DSPM01  LSR TMP0+1
        ROR TMP0
        DEX 
        BNE DSPM01
DSPBYT  JSR STOP            ; check for stop key
        BEQ DSPMX           ; exit early if pressed
        JSR DISPMEM         ; display 1 line containing 8 bytes
        LDA #8              ; increase start address by 8 bytes
        JSR BUMPAD2
        JSR SUBA1           ; decrement line counter
        BCS DSPBYT          ; show another line until it's < 0
DSPMX   JMP STRT            ; back to main loop
MERROR  JMP ERROR           ; handle error

; -----------------------------------------------------------------------------
; alter registers [;]
ALTR    JSR COPY1P          ; store first parameter in PC
        LDY #0              ; init counter
ALTR1   JSR GETPAR          ; get value for next register
        BCS ALTRX           ; exit early if no more values given
        LDA TMP0            ; store in memory, offset from SR
        STA SR,Y            ; these locations will be transferred to the
        INY                 ;   actual registers before exiting the monitor
        CPY #$05            ; have we updated all 5 yet?
        BCC ALTR1           ; if not, get next
ALTRX   JMP STRT            ; back to main loop

; -----------------------------------------------------------------------------
; alter memory [>]
ALTM    BCS ALTMX           ; exit if no parameter provided
        JSR COPY12          ; copy parameter to start address
        LDY #0
ALTM1   JSR GETPAR          ; get value for next byte of memory
        BCS ALTMX           ; if none given, exit early
        LDA TMP0            ; poke value into memory at start address + Y
        STA (TMP2),Y
        INY                 ; next byte
        CPY #8              ; have we read 8 bytes yet?
        BCC ALTM1           ; if not, read the next one
ALTMX   LDA #$91            ; move cursor up
        JSR CHROUT
        JSR DISPMEM         ; re-display line to make ascii match hex
        JMP STRT            ; back to main loop

; -----------------------------------------------------------------------------
; goto (run) [G]
GOTO    LDX SP              ; load stack pointer from memory
        TXS                 ; save in SP register
GOTO2   JSR COPY1P          ; copy provided address to PC
        SEI                 ; disable interrupts
        LDA PCH             ; push PC high byte on stack
        PHA
        LDA PCL             ; push PC low byte on stack
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

; -----------------------------------------------------------------------------
; display 8 bytes of memory
DISPMEM JSR CRLF            ; new line
        LDA #">"            ; prefix > so memory can be edited in place
        JSR CHROUT
        JSR SHOWAD          ; show address of first byte on line
        LDY #0
        BEQ DMEMGO          ; SHOWAD already printed a space after the address
DMEMLP  JSR SPACE           ; print space between bytes
DMEMGO  LDA (TMP2),Y        ; load byte from start address + Y
        JSR WRTWO           ; output hex digits for byte
        INY                 ; next byte
        CPY #8              ; have we output 8 bytes yet?
        BCC DMEMLP          ; if not, output next byte
        LDY #MSG5-MSGBAS    ; if so, output : and turn on reverse video
        JSR SNDMSG          ;   before displaying ascii representation
        LDY #0              ; back to first byte in line
DCHAR   LDA (TMP2),Y        ; load byte at start address + Y
        TAX                 ; stash in X
        AND #$BF            ; clear 6th bit
        CMP #$22            ; is it a quote (")?
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

; -----------------------------------------------------------------------------
; compare memory [C]
COMPAR  LDA #0              ; bit 7 clear signals compare
        .BYTE $2C           ; absolute BIT opcode consumes next word (LDA #$80)

; transfer memory [T]
TRANS   LDA #$80            ; bit 7 set signals transfer
        STA SAVY            ; save compare/transfer flag in SAVY
        LDA #0              ; assume we're counting up (bit 7 clear)
        STA UPFLG           ; save direction flag
        JSR GETDIF          ; get two addresses and calculate difference
                            ;   TMP2 = source start
                            ;   STASH = source end
                            ;   STORE = length
        BCS TERROR          ; carry set indicates error
        JSR GETPAR          ; get destination address in TMP0
        BCC TOKAY           ; carry set indicates error
TERROR  JMP ERROR           ; handle error
TOKAY   BIT SAVY            ; transfer or compare?
        BPL COMPAR1         ; high bit clear indicates compare
        LDA TMP2            ; if it's a transfer, we must take steps
        CMP TMP0            ;   to avoid overwriting the source bytes before 
        LDA TMP2+1          ;   they have been transferred
        SBC TMP0+1          ; compare source (TMP2) to destination (TMP0)
        BCS COMPAR1         ; and count up if source is before than desitnation
        LDA STORE           ; otherwise, start at end and count down...
        ADC TMP0            ; add length (STORE) to desintation (TMP0)
        STA TMP0            ; to calculate end of destination
        LDA STORE+1
        ADC TMP0+1
        STA TMP0+1
        LDX #1              ; change source pointer from beginning to end
TDOWN   LDA STASH,X         ; TMP2 = source end (STASH)
        STA TMP2,X
        DEX 
        BPL TDOWN
        LDA #$80            ; high bit set in UPFLG means count down
        STA UPFLG
COMPAR1 JSR CRLF            ; new line
        LDY #0              ; no offset from pointer
TCLOOP  JSR STOP            ; check for stop key
        BEQ TEXIT           ; exit if pressed
        LDA (TMP2),Y        ; load byte from source
        BIT SAVY            ; transfer or compare?
        BPL COMPAR2         ; skip store if comparing
        STA (TMP0),Y        ; otherwise, store in destination
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
TEXIT   JMP STRT            ; back to main loop

; -----------------------------------------------------------------------------
; hunt memory [H]
HUNT    JSR GETDIF          ; get start (TMP2) and end (TMP0) of haystack
        BCS HERROR          ; carry indicates error
        LDY #0
        JSR GETCHR          ; get a single character
        CMP #"'"            ; is it a single quote?
        BNE NOSTRH          ; if not, input needle as hex bytes
        JSR GETCHR          ; if so, input needle as string
        CMP #0
        BEQ HERROR          ; error if needle isn't at least one byte
HPAR    STA STAGE,Y         ; save char in staging area
        INY 
        JSR GETCHR          ; get another char
        BEQ HTGO            ; if it's null start searching
        CPY #ESTAGE-STAGE   ; have we filled up the needle staging area?
        BNE HPAR            ; if not, get another character
        BEQ HTGO            ; if so, start searching
NOSTRH  JSR RDPAR           ; read hex bytes if string not indicated
HLP     LDA TMP0            ; save last read byte in staging area
        STA STAGE,Y
        INY                 ; get another hex byte
        JSR GETPAR
        BCS HTGO            ; if there is none, start searching
        CPY #ESTAGE-STAGE   ; have we filled up the needle staging area?
        BNE HLP             ; if not, get another byte
HTGO    STY SAVY            ; save length of needle
        JSR CRLF            ; new line
HSCAN   LDY #0
HLP3    LDA (TMP2),Y        ; get first byte in haystack
        CMP STAGE,Y         ; compare it to first byte of needle
        BNE HNOFT           ; if it doesn't match, we haven't found anything
        INY                 ; if it does, check the next byte
        CPY SAVY            ; have we reached the end of the needle?
        BNE HLP3            ; if not, keep comparing bytes
        JSR SHOWAD          ; match found, show address
HNOFT   JSR STOP            ; no match, check for stop key
        BEQ HEXIT           ; exit prematurely if pressed
        JSR ADDA2           ; increment haystack pointer
        JSR SUB13           ; decrement haystack length
        BCS HSCAN           ; still more haystack? keep searching
HEXIT   JMP STRT            ; back to main loop
HERROR  JMP ERROR           ; handle error

; -----------------------------------------------------------------------------
; load, save, or verify [LSV]
LD      LDY #1              ; default to reading from tape, device #1
        STY FA
        STY SADD            ; default to secondary address #1
        DEY
        STY FNLEN           ; start with an empty filename
        STY SATUS           ; clear status
        LDA #>STAGE         ; set filename pointer to staging buffer
        STA FNADR+1
        LDA #<STAGE
        STA FNADR
L1      JSR GETCHR          ; get a character
        BEQ LSHORT          ; no filename given, try load or verify from tape
        CMP #$20            ; skip leading spaces
        BEQ L1
        CMP #$22            ; error if filename doesn't start with a quote
        BNE LERROR
        LDX CHRPNT          ; load current char pointer into index reg
L3      LDA INBUFF,X        ; load current char from buffer to accumulator
        BEQ LSHORT          ; no filename given, try load or verify from tape
        INX                 ; next char
        CMP #$22            ; is it a quote?
        BEQ L8              ; if so, we've reached the end of the filename
        STA (FNADR),Y       ; if not, save character in filename buffer
        INC FNLEN           ; increment filename length
        INY 
        CPY #ESTAGE-STAGE   ; check whether buffer is full
        BCC L3              ; if not, get another character
LERROR  JMP ERROR           ; if so, handle error
L8      STX CHRPNT          ; set character pointer to the current index
        JSR GETCHR          ; eat separator between filename and device #
        BEQ LSHORT          ; no separator, try to load or verify from tape
        JSR GETPAR          ; get device number
        BCS LSHORT          ; no device # given, try load or verify from tape
        LDA TMP0            ; set device number for kernal routines
        STA FA
        JSR GETPAR          ; get start address for load or save in TMP0
        BCS LSHORT          ; no start address, try to load or verify
        JSR COPY12          ; transfer start address to TMP2
        JSR GETPAR          ; get end address for save in TMP0
        BCS LDADDR          ; no end address, try to load to given start addr
        JSR CRLF            ; new line
        LDX TMP0            ; put low byte of end address in X
        LDY TMP0+1          ; put high byte of end address in Y
        LDA SAVY            ; confirm that we're doing a save
        CMP #"S"
        BNE LERROR          ; if not, error due to too many params
        LDA #0
        STA SADD            ; set secondary address to 0
        LDA #TMP2           ; put addr of zero-page pointer to data in A
        JSR SAVE            ; call kernal save routine
LSVXIT  JMP STRT            ; back to mainloop
LSHORT  LDA SAVY            ; check which command we received
        CMP #"V"
        BEQ LOADIT          ; we're doing a verify so don't set A to 0
        CMP #"L"
        BNE LERROR          ; error due to not enough params for save
        LDA #0              ; 0 in A signals load, anything else is verify
LOADIT  JSR LOAD            ; call kernal load routine
        LDA SATUS           ; get i/o status
        AND #$10            ; check bit 5 for checksum error
        BEQ LSVXIT          ; if no error go back to mainloop
        LDA SAVY            ; ?? not sure what these two lines are for...
        BEQ LERROR          ; ?? SAVY will never be 0, so why check?
        LDY #MSG6-MSGBAS    ; display "ERROR" if checksum didn't match
        JSR SNDMSG
        JMP STRT            ; back to mainloop
LDADDR  LDX TMP2            ; load address low byte in X
        LDY TMP2+1          ; load address high byte in Y
        LDA #0              ; 0 in A signals load
        STA SADD            ; secondary addr 0 means load to addr in X and Y
        BEQ LSHORT          ; execute load

; -----------------------------------------------------------------------------
; fill memory [F]
FILL    JSR GETDIF          ; start in TMP2, end in STASH, length in STORE
        BCS AERROR          ; carry set indicates error
        JSR GETPAR          ; get value to fill in TMP0
        BCS AERROR          ; carry set indicates error
        JSR GETCHR          ; any more characters triggers an error
        BNE AERROR
        LDY #0              ; no offset
FILLP   LDA TMP0            ; load value to fill in accumulator
        STA (TMP2),Y        ; store fill value in current address
        JSR STOP            ; check for stop key
        BEQ FSTART          ; if pressed, back to main loop
        JSR ADDA2           ; increment address
        JSR SUB13           ; decrement length
        BCS FILLP           ; keep going until length reaches 0
FSTART  JMP STRT            ; back to main loop

; -----------------------------------------------------------------------------
; assemble [A.]

; read in mnemonic
ASSEM   BCS AERROR          ; error if no address given
        JSR COPY12          ; copy address to TMP2
AGET1   LDX #0
        STX U0AA0+1         ; clear byte that mnemonic gets shifted into
        STX DIGCNT          ; clear digit count
AGET2   JSR GETCHR          ; get a char
        BNE ALMOR           ; proceed if the character isn't null
        CPX #0              ; it's null, have read a mnemonic yet?
        BEQ FSTART          ; if not, silently go back to main loop
ALMOR   CMP #$20            ; skip leading spaces
        BEQ AGET1
        STA MNEMW,X         ; put character in mnemonic buffer
        INX
        CPX #3              ; have we read 3 characters yet?
        BNE AGET2           ; if not, get next character

; compress mnemonic into two bytes
ASQEEZ  DEX                 ; move to previous char
        BMI AOPRND          ; if we're done with mnemonic, look for operand
        LDA MNEMW,X         ; get current character
        SEC                 ; pack 3-letter mnemonic into 2 bytes (15 bits)
        SBC #$3F            ; subtract $3F from ascii code so A-Z = 2 to 27
        LDY #$05            ; letters now fit in 5 bits; shift them out
ASHIFT  LSR A               ;   into the first two bytes of the inst buffer
        ROR U0AA0+1         ; catch the low bit from accumulator in right byte
        ROR U0AA0           ; catch the low bit from right byte in left byte
        DEY                 ; count down bits
        BNE ASHIFT          ; keep looping until we reach zero
        BEQ ASQEEZ          ; unconditional branch to handle next char
AERROR  JMP ERROR           ; handle error

; parse operand
AOPRND  LDX #2              ; mnemonic is in first two bytes so start at third
ASCAN   LDA DIGCNT          ; did we find address digits last time?
        BNE AFORM1          ; if so, look for mode chars
        JSR RDVAL           ; otherwise, look for an address
        BEQ AFORM0          ; we didn't find an address, look for characters
        BCS AERROR          ; carry flag indicates error
        LDA #"$"
        STA U0AA0,X         ; prefix addresses with $
        INX                 ; next position in buffer
        LDY #4              ; non-zero page addresses are 4 hex digits
        LDA NUMBIT          ; check numeric base in which address was given
        CMP #8              ; for addresses given in octal or binary
        BCC AADDR           ;   use only the high byte to determine page
        CPY DIGCNT          ; for decimal or hex, force non-zero page addressing
        BEQ AFILL0          ;   if address was given with four digits or more 
AADDR   LDA TMP0+1          ; check whether high byte of address is zero
        BNE AFILL0          ; non-zero high byte means we're not in zero page
        LDY #2              ; if it's in zero page, addr is 2 hex digits
AFILL0  LDA #$30            ; use 0 as placeholder for each hex digit in addr
AFIL0L  STA U0AA0,X         ; put placeholder in assembly buffer
        INX                 ; move to next byte in buffer
        DEY                 ; decrement number of remaining digits
        BNE AFIL0L          ; loop until all digits have been placed
AFORM0  DEC CHRPNT          ; non-numeric input; back 1 char to see what it was
AFORM1  JSR GETCHR          ; get next character
        BEQ AESCAN          ; if there is none, we're finished scanning
        CMP #$20            ; skip spaces
        BEQ ASCAN
        STA U0AA0,X         ; store character in assembly buffer
        INX                 ; move to next byte in buffer
        CPX #U0AAE-U0AA0    ; is instruction buffer full?
        BCC ASCAN           ; if not, keep scanning
        BCS AERROR          ; error if buffer is full

; find matching opcode
AESCAN  STX STORE           ; save number of bytes in assembly buffer
        LDX #0              ; start at opcode $00 and check every one until
        STX OPCODE          ;   we find one that matches our criteria
ATRYOP  LDX #0
        STX U9F             ; reset index into work buffer
        LDA OPCODE
        JSR INSTXX          ; look up instruction format for current opcode
        LDX ACMD            ; save addressing command for later
        STX STORE+1
        TAX                 ; use current opcode as index
        LDA MNEMR,X         ; check right byte of compressed mnemonic
        JSR CHEKOP
        LDA MNEML,X         ; check left byte of compressed mnemonic
        JSR CHEKOP
        LDX #6              ; 6 possible characters to check against operand
TRYIT   CPX #3              ; are we on character 3?
        BNE TRYMOD          ; if not, check operand characters
        LDY LENGTH          ; otherwise, check number of bytes in operand
        BEQ TRYMOD          ; if zero, check operand characters
TRYAD   LDA ACMD            ; otherwise, look for an address
        CMP #$E8            ; special case for relative addressing mode
                            ;   since it's specified with 4 digits in assembly
                            ;   but encoded with only 1 byte in object code
        LDA #$30            ; '0' is the digit placeholder we're looking for
        BCS TRY4B           ; ACMD >= $E8 indicates relative addressing
        JSR CHEK2B          ; ACMD < $E8 indicates normal addressing
        DEY                 ; consume byte
        BNE TRYAD           ; check for 2 more digits if not zero-page
TRYMOD  ASL ACMD            ; shift a bit out of the addressing command
        BCC UB4DF           ; if it's zero, skip checking current character
        LDA CHAR1-1,X
        JSR CHEKOP          ; otherwise first character against operand
        LDA CHAR2-1,X       ; get second character to check
        BEQ UB4DF           ; if it's zero, skip checking it
        JSR CHEKOP          ; otherwise check it against hte operand
UB4DF   DEX                 ; move to next character
        BNE TRYIT           ; repeat tests
        BEQ TRYBRAN
TRY4B   JSR CHEK2B          ; check for 4 digit address placeholder
        JSR CHEK2B          ;   by checking for 2 digits twice
TRYBRAN LDA STORE           ; get number of bytes in assembly buffer
        CMP U9F             ; more bytes left to check?
        BEQ ABRAN           ; if not, we've found a match; build instruction
        JMP BUMPOP          ; if so, this opcode doesn't match; try the next

; convert branches to relative address
ABRAN   LDY LENGTH          ; get number of bytes in operand
        BEQ A1BYTE          ; if none, just output the opcode
        LDA STORE+1         ; otherwise check the address format
        CMP #$9D            ; is it a relative branch?
        BNE OBJPUT          ; if not, skip relative branch calculation
        LDA TMP0            ; calculate the difference between the current
        SBC TMP2            ;   address and the branch target (low byte)
        TAX                 ; save it in X
        LDA TMP0+1          ; borrow from the high byte if necessary
        SBC TMP2+1
        BCC ABBACK          ; if result is negative, we're branching back
        BNE SERROR          ; high bytes must be equal when branching forward
        CPX #$82            ; difference between low bytes must be < 130
        BCS SERROR          ; error if the address is too far away
        BCC ABRANX
ABBACK  TAY                 ; when branching backward high byte of target must
        INY                 ;   be 1 less than high byte of current address
        BNE SERROR          ; if not, it's too far away
        CPX #$82            ; difference between low bytes must be < 130
        BCC SERROR          ; if not, it's too far away
ABRANX  DEX                 ; adjust branch target relative to the 
        DEX                 ;   instruction following this one
        TXA
        LDY LENGTH          ; load length of operand
        BNE OBJP2           ; don't use the absolute address

; assemble machine code
OBJPUT  LDA TMP0-1,Y        ; get the operand
OBJP2   STA (TMP2),Y        ; store it after the opcode
        DEY
        BNE OBJPUT          ; copy the other byte of operand if there is one
A1BYTE  LDA OPCODE          ; put opcode into instruction
        STA (TMP2),Y
        JSR CRLF            ; carriage return
        LDA #$91            ; back up one line
        JSR CHROUT
        LDY #MSG7-MSGBAS    ; "A " prefix
        JSR SNDCLR          ; clear line
        JSR DISLIN          ; disassemble the instruction we just assembled
        INC LENGTH          ; instruction length = operand length + 1 byte
        LDA LENGTH          ;   for the opcode
        JSR BUMPAD2         ; increment address by length of instruction
        LDA #"A"            ; stuff keyboard buffer with next assemble command:
        STA KEYD            ;   "A XXXX " where XXXX is the next address
        LDA #" "            ;   after the previously assembled instruction
        STA KEYD+1
        STA KEYD+6
        LDA TMP2+1          ; convert high byte of next address to hex
        JSR ASCTWO
        STA KEYD+2          ; put it in the keyboard buffer
        STX KEYD+3
        LDA TMP2            ; convert low byte of next address to hex
        JSR ASCTWO
        STA KEYD+4          ; put it in the keyboard buffer
        STX KEYD+5
        LDA #7              ; set number of chars in keyboard buffer
        STA NDX
        JMP STRT            ; back to main loop
SERROR  JMP ERROR           ; handle error

; check characters in operand
CHEK2B  JSR CHEKOP          ; check two bytes against value in accumulator
CHEKOP  STX SAVX            ; stash X
        LDX U9F             ; get current index into work buffer
        CMP U0AA0,X         ; check whether this opcode matches the buffer
        BEQ OPOK            ;   matching so far, check the next criteria
        PLA                 ; didn't match, so throw away return address
        PLA                 ;   on the stack because we're starting over
BUMPOP  INC OPCODE          ; check the next opcode
        BEQ SERROR          ; error if we tried every opcode and none fit
        JMP ATRYOP          ; start over with new opcode
OPOK    INC U9F             ; opcode matches so far; check the next criteria
        LDX SAVX            ; restore X
        RTS

; -----------------------------------------------------------------------------
; disassemble [D]
DISASS  BCS DIS0AD          ; if no address was given, start from last address
        JSR COPY12          ; copy start address to TMP2
        JSR GETPAR          ; get end address in TMP0
        BCC DIS2AD          ; if one was given, skip default
DIS0AD  LDA #$14            ; disassemble 14 bytes by default
        STA TMP0            ; store length in TMP0
        BNE DISGO           ; skip length calculation
DIS2AD  JSR SUB12           ; calculate number of bytes between start and end
        BCC DERROR          ; error if end address is before start address
DISGO   JSR CLINE           ; clear the current line
        JSR STOP            ; check for stop key
        BEQ DISEXIT         ; exit early if pressed
        JSR DSOUT1          ; output disassembly prefix ". "
        INC LENGTH
        LDA LENGTH          ; add length of last instruction to start address
        JSR BUMPAD2
        LDA LENGTH          ; subtract length of last inst from end address
        JSR SUBA2
        BCS DISGO
DISEXIT JMP STRT            ; back to mainloop
DERROR  JMP ERROR

DSOUT1  LDA #"."            ; output ". " prefix to allow edit and reassemble
        JSR CHROUT
        JSR SPACE

DISLIN  JSR SHOWAD          ; show the address of the instruction
        JSR SPACE           ; insert a space
        LDY #0              ; no offset
        LDA (TMP2),Y        ; load operand of current instruction
        JSR INSTXX          ; get mnemonic and addressing mode for opcode
        PHA                 ; save index into mnemonic table
        LDX LENGTH          ; get length of operand
        INX                 ; add 1 byte for opcode
DSBYT   DEX                 ; decrement index
        BPL DSHEX           ; show hex for byte being disassembled
        STY SAVY            ; save index
        LDY #MSG8-MSGBAS    ; skip 3 spaces
        JSR SNDMSG
        LDY SAVY            ; restore index
        JMP NXBYT
DSHEX   LDA (TMP2),Y        ; show hex for byte
        JSR WRBYTE

NXBYT   INY                 ; next byte
        CPY #3              ; have we output 3 bytes yet?
        BCC DSBYT           ; if not, loop
        PLA                 ; restore index into mnemonic table
        LDX #3              ; 3 letters in mnemonic
        JSR PROPXX          ; print mnemonic
        LDX #6              ; 6 possible address mode character combos
PRADR1  CPX #3              ; have we checked the third combo yet?
        BNE PRADR3          ; if so, output the leading characters
        LDY LENGTH          ; get the length of the operand
        BEQ PRADR3          ; if it's zero, there's no operand to print
PRADR2  LDA ACMD            ; otherwise, get the addressing mode
        CMP #$E8            ; check for relative addressing
        PHP                 ; save result of check
        LDA (TMP2),Y        ; get the operand
        PLP                 ; restore result of check
        BCS RELAD           ; handle a relative address
        JSR WRTWO           ; output digits from address
        DEY
        BNE PRADR2          ; repeat for next byte of operand, if there is one
PRADR3  ASL ACMD            ; check whether addr mode uses the current char
        BCC PRADR4          ; if not, skip it
        LDA CHAR1-1,X       ; look up the first char in the table
        JSR CHROUT          ; print first char
        LDA CHAR2-1,X       ; look up the second char in the table
        BEQ PRADR4          ; if there's no second character, skip it
        JSR CHROUT          ; print second char
PRADR4  DEX                 ; next potential address mode character
        BNE PRADR1          ; loop if we haven't checked them all yet
        RTS                 ; back to caller
RELAD   JSR UB64D           ; calculate absolute address from relative
        CLC
        ADC #1              ; adjust address relative to next instruction
        BNE RELEND          ; don't increment high byte unless we overflowed
        INX                 ; increment high byte
RELEND  JMP WRADDR          ; print address

UB64D   LDX TMP2+1          ; get high byte of current address
        TAY                 ; is relative address positive or negative?
        BPL RELC2           ; if positive, leave high byte alone
        DEX                 ; if negative, decrement high byte
RELC2   ADC TMP2            ; add relative address to low byte
        BCC RELC3           ; if there's no carry, we're done
        INX                 ; if there's a carry, increment the high byte
RELC3   RTS

; -----------------------------------------------------------------------------
; get opcode mode and length

; Note: the labels are different, but the code of this subroutine is almost
; identical to the INSDS2 subroutine of the Apple Mini-Assembler on page 78 of
; the Apple II Red Book. I'm not sure exactly where this code originated
; (MOS or Apple) but it's clear that this part of Supermon64 and the 
; Mini-Asssembler share a common heritage.  The comments showing the way the 
; opcodes are transformed into indexes for the mnemonic lookup table come
; from the Mini-Assembler source.

INSTXX  TAY                 ; stash opcode in accumulator in Y for later
        LSR A               ; is opcode even or odd?
        BCC IEVEN
        LSR A
        BCS ERR             ; invalid opcodes XXXXXX11
        CMP #$22
        BEQ ERR             ; invalid opcode 10001001
        AND #$07            ; mask bits to 10000XXX
        ORA #$80
IEVEN   LSR A               ; LSB determines whether to use left/right nybble
        TAX                 ; get format index using remaining high bytes
        LDA MODE,X
        BCS RTMODE          ; look at left or right nybble based on carry bit
        LSR A               ; if carry = 0, use left nybble
        LSR A
        LSR A
        LSR A
RTMODE  AND #$0F            ; if carry = 1, use right nybble
        BNE GETFMT
ERR     LDY #$80            ; substitute 10000000 for invalid opcodes
        LDA #0
GETFMT  TAX
        LDA MODE2,X         ; lookup operand format using selected nybble
        STA ACMD            ; save for later use
        AND #$03            ; lower 2 bits indicate number of bytes in operand
        STA LENGTH
        TYA                 ; restore original opcode
        AND #$8F            ; mask bits to X000XXXX
        TAX                 ; save it
        TYA                 ; restore original opcode
        LDY #3
        CPX #$8A            ; check if opcode = 1XXX1010
        BEQ GTFM4
GTFM2   LSR A               ; transform opcode into index for mnemonic table
        BCC GTFM4
        LSR A               ; opcodes transformed as follows:
GTFM3   LSR A               ; 1XXX1010->00101XXX
        ORA #$20            ; XXXYYY01->00111XXX
        DEY                 ; XXXYYY10->00111XXX
        BNE GTFM3           ; XXXYY100->00110XXX
        INY                 ; XXXXX000->000XXXXX
GTFM4   DEY
        BNE GTFM2
        RTS

; -----------------------------------------------------------------------------
; extract and print packed mnemonics
PROPXX  TAY                 ; use index in accumulator to look up mnemonic
        LDA MNEML,Y         ;   and place a temporary copy in STORE
        STA STORE
        LDA MNEMR,Y
        STA STORE+1
PRMN1   LDA #0              ; clear accumulator
        LDY #$05            ; shift 5 times
PRMN2   ASL STORE+1         ; shift right byte
        ROL STORE           ; rotate bits from right byte into left byte
        ROL A               ; rotate bits from left byte into accumulator
        DEY                 ; next bit
        BNE PRMN2           ; loop until all bits shifted
        ADC #$3F            ; calculate ascii code for letter by adding to '?'
        JSR CHROUT          ; output letter
        DEX                 ; next letter
        BNE PRMN1           ; loop until all 3 letters are output
        JMP SPACE           ; output space

; -----------------------------------------------------------------------------
; read parameters
RDPAR   DEC CHRPNT          ; back up one char
GETPAR  JSR RDVAL           ; read the value
        BCS GTERR           ; carry set indicates error
        JSR GOTCHR          ; check previous character
        BNE CKTERM          ; if it's not null, check if it's a valid separator
        DEC CHRPNT          ; back up one char
        LDA DIGCNT          ; get number of digits read
        BNE GETGOT          ; found some digits
        BEQ GTNIL           ; didn't find any digits
CKTERM  CMP #$20            ; space or comma are valid separators
        BEQ GETGOT          ; anything else is an error
        CMP #","
        BEQ GETGOT
GTERR   PLA                 ; encountered error
        PLA                 ; get rid of command vector pushed on stack
        JMP ERROR           ; handle error
GTNIL   SEC                 ; set carry to indicate no parameter found
        .BYTE $24           ; BIT ZP opcode consumes next byte (CLC)
GETGOT  CLC                 ; clear carry to indicate paremeter returned
        LDA DIGCNT          ; return number of digits in A
        RTS                 ; return to address pushed from vector table

; -----------------------------------------------------------------------------
; read a value in the specified base
RDVAL   LDA #0              ; clear temp
        STA TMP0
        STA TMP0+1
        STA DIGCNT          ; clear digit counter
        TXA                 ; save X and Y
        PHA
        TYA
        PHA
RDVMOR  JSR GETCHR          ; get next character from input buffer
        BEQ RDNILK          ; null at end of buffer
        CMP #$20            ; skip spaces
        BEQ RDVMOR
        LDX #3              ; check numeric base [$+&%]
GNMODE  CMP HIKEY,X
        BEQ GOTMOD          ; got a match, set up base
        DEX
        BPL GNMODE          ; check next base
        INX                 ; default to hex
        DEC CHRPNT          ; back up one character
GOTMOD  LDY MODTAB,X        ; get base value
        LDA LENTAB,X        ; get bits per digit
        STA NUMBIT          ; store bits per digit 
NUDIG   JSR GETCHR          ; get next char in A
RDNILK  BEQ RDNIL           ; end of number if no more characters
        SEC
        SBC #$30            ; subtract ascii value of 0 to get numeric value
        BCC RDNIL           ; end of number if character was less than 0
        CMP #$0A
        BCC DIGMOR          ; not a hex digit if less than A
        SBC #$07            ; 7 chars between ascii 9 and A, so subtract 7
        CMP #$10            ; end of number if char is greater than F
        BCS RDNIL
DIGMOR  STA INDIG           ; store the digit
        CPY INDIG           ; compare base with the digit
        BCC RDERR           ; error if the digit >= the base
        BEQ RDERR
        INC DIGCNT          ; increment the number of digits
        CPY #10
        BNE NODECM          ; skip the next part if not using base 10
        LDX #1
DECLP1  LDA TMP0,X          ; stash the previous 16-bit value for later use
        STA STASH,X
        DEX
        BPL DECLP1
NODECM  LDX NUMBIT          ; number of bits to shift
TIMES2  ASL TMP0            ; shift 16-bit value by specified number of bits
        ROL TMP0+1
        BCS RDERR           ; error if we overflowed 16 bits
        DEX
        BNE TIMES2          ; shift remaining bits
        CPY #10
        BNE NODEC2          ; skip the next part if not using base 10
        ASL STASH           ; shift the previous 16-bit value one bit left
        ROL STASH+1
        BCS RDERR           ; error if we overflowed 16 bits
        LDA STASH           ; add shifted previous value to current value
        ADC TMP0
        STA TMP0
        LDA STASH+1
        ADC TMP0+1
        STA TMP0+1
        BCS RDERR           ; error if we overflowed 16 bits
NODEC2  CLC 
        LDA INDIG           ; load current digit
        ADC TMP0            ; add current digit to low byte
        STA TMP0            ; and store result back in low byte
        TXA                 ; A=0
        ADC TMP0+1          ; add carry to high byte
        STA TMP0+1          ; and store result back in high byte
        BCC NUDIG           ; get next digit if we didn't overflow
RDERR   SEC                 ; set carry to indicate error
        .BYTE $24           ; BIT ZP opcode consumes next byte (CLC)
RDNIL   CLC                 ; clear carry to indicate success
        STY NUMBIT          ; save base of number
        PLA                 ; restore X and Y
        TAY
        PLA
        TAX
        LDA DIGCNT          ; return number of digits in A
        RTS

; -----------------------------------------------------------------------------
; print address
SHOWAD  LDA TMP2
        LDX TMP2+1

WRADDR  PHA                 ; save low byte
        TXA                 ; put high byte in A
        JSR WRTWO           ; output high byte
        PLA                 ; restore low byte

WRBYTE  JSR WRTWO           ; output byte in A

SPACE   LDA #$20            ; output space
        BNE FLIP

CHOUT   CMP #$0D            ; output char with special handling of CR
        BNE FLIP
CRLF    LDA #$0D            ; load CR in A
        BIT $13             ; check default channel
        BPL FLIP            ; if high bit is clear output CR only
        JSR CHROUT          ; otherwise output CR+LF
        LDA #$0A            ; output LF
FLIP    JMP CHROUT

FRESH   JSR CRLF            ; output CR
        LDA #$20            ; load space in A
        JSR CHROUT
        JMP SNCLR

; -----------------------------------------------------------------------------
; output two hex digits for byte
WRTWO   STX SAVX            ; save X
        JSR ASCTWO          ; get hex chars for byte in X (lower) and A (upper)
        JSR CHROUT          ; output upper nybble
        TXA                 ; transfer lower to A
        LDX SAVX            ; restore X
        JMP CHROUT          ; output lower nybble

; -----------------------------------------------------------------------------
; convert byte in A to hex digits
ASCTWO  PHA                 ; save byte
        JSR ASCII           ; do low nybble
        TAX                 ; save in X
        PLA                 ; restore byte
        LSR A               ; shift upper nybble down
        LSR A
        LSR A
        LSR A

; convert low nybble in A to hex digit
ASCII   AND #$0F            ; clear upper nibble
        CMP #$0A            ; if less than A, skip next step
        BCC ASC1
        ADC #6              ; skip ascii chars between 9 and A
ASC1    ADC #$30            ; add ascii char 0 to value
        RTS

; -----------------------------------------------------------------------------
; get prev char from input buffer
GOTCHR  DEC CHRPNT

; get next char from input buffer
GETCHR  STX SAVX
        LDX CHRPNT          ; get pointer to next char
        LDA INBUFF,X        ; load next char in A
        BEQ NOCHAR          ; null, :, or ? signal end of buffer
        CMP #":"        
        BEQ NOCHAR
        CMP #"?"
NOCHAR  PHP
        INC CHRPNT          ; next char
        LDX SAVX
        PLP                 ; Z flag will signal last character
        RTS

; -----------------------------------------------------------------------------
; copy TMP0 to TMP2
COPY12  LDA TMP0            ; low byte
        STA TMP2
        LDA TMP0+1          ; high byte
        STA TMP2+1
        RTS

; -----------------------------------------------------------------------------
; subtract TMP2 from TMP0
SUB12   SEC
        LDA TMP0            ; subtract low byte
        SBC TMP2
        STA TMP0
        LDA TMP0+1
        SBC TMP2+1          ; subtract high byte
        STA TMP0+1
        RTS

; -----------------------------------------------------------------------------
; subtract from TMP0
SUBA1   LDA #1              ; shortcut to decrement by 1
SUBA2   STA SAVX            ; subtrahend in accumulator
        SEC
        LDA TMP0            ; minuend in low byte
        SBC SAVX
        STA TMP0
        LDA TMP0+1          ; borrow from high byte
        SBC #0
        STA TMP0+1
        RTS

; -----------------------------------------------------------------------------
; subtract 1 from STORE
SUB13   SEC
        LDA STORE
        SBC #1              ; decrement low byte
        STA STORE
        LDA STORE+1
        SBC #0              ; borrow from high byte
        STA STORE+1
        RTS

; -----------------------------------------------------------------------------
; add to TMP2
ADDA2   LDA #1              ; shortcut to increment by 1
BUMPAD2 CLC
        ADC TMP2            ; add value in accumulator to low byte
        STA TMP2
        BCC BUMPEX
        INC TMP2+1          ; carry to high byte
BUMPEX  RTS 

; -----------------------------------------------------------------------------
; subtract 1 from TMP2
SUB21   SEC
        LDA TMP2            ; decrement low byte
        SBC #1
        STA TMP2
        LDA TMP2+1          ; borrow from high byte
        SBC #0
        STA TMP2+1
        RTS

; -----------------------------------------------------------------------------
; copy TMP0 to PC
COPY1P  BCS CPY1PX          ; do nothing if parameter is empty
        LDA TMP0            ; copy low byte
        LDY TMP0+1          ; copy high byte
        STA PCL
        STY PCH
CPY1PX  RTS 

; -----------------------------------------------------------------------------
; get start/end addresses and calc difference
GETDIF  BCS GDIFX           ; exit with error if no parameter given
        JSR COPY12          ; save start address in TMP2
        JSR GETPAR          ; get end address in TMP0
        BCS GDIFX           ; exit with error if no parameter given
        LDA TMP0            ; save end address in STASH
        STA STASH
        LDA TMP0+1
        STA STASH+1
        JSR SUB12           ; subtract start address from end address
        LDA TMP0
        STA STORE           ; save difference in STORE
        LDA TMP0+1
        STA STORE+1
        BCC GDIFX           ; error if start address is after end address
        CLC                 ; clear carry to indicate success
        .BYTE $24           ; BIT ZP opcode consumes next byte (SEC)
GDIFX   SEC                 ; set carry to indicate error
        RTS

; -----------------------------------------------------------------------------
; convert base [$+&%]
CONVRT  JSR RDPAR           ; read a parameter
        JSR FRESH           ; next line and clear
        LDA #"$"            ; output $ sigil for hex
        JSR CHROUT
        LDA TMP0            ; load the 16-bit value entered
        LDX TMP0+1
        JSR WRADDR          ; print it in 4 hex digits
        JSR FRESH
        LDA #"+"            ; output + sigil for decimal
        JSR CHROUT
        JSR CVTDEC          ; convert to BCD using hardware mode
        LDA #0              ; clear digit counter
        LDX #6              ; max digits + 1
        LDY #3              ; bits per digit - 1
        JSR NMPRNT          ; print result without leading zeros
        JSR FRESH           ; next line and clear
        LDA #"&"            ; print & sigil for octal
        JSR CHROUT
        LDA #0              ; clear digit counter
        LDX #8              ; max digits + 1
        LDY #2              ; bits per digit - 1
        JSR PRINUM          ; output number
        JSR FRESH           ; next line and clear
        LDA #"%"            ; print % sigil for binary
        JSR CHROUT
        LDA #0              ; clear digit counter
        LDX #$18            ; max digits + 1
        LDY #0              ; bits per digit - 1
        JSR PRINUM          ; output number
        JMP STRT            ; back to mainloop

; -----------------------------------------------------------------------------
; convert binary to BCD

CVTDEC  JSR COPY12          ; copy value from TMP0 to TMP2
        LDA #0
        LDX #2              ; clear 3 bytes in work buffer
DECML1  STA U0AA0,X
        DEX
        BPL DECML1
        LDY #16             ; 16 bits in input
        PHP                 ; save status register
        SEI                 ; make sure no interrupts occur with BCD enabled
        SED
DECML2  ASL TMP2            ; rotate bytes out of input low byte
        ROL TMP2+1          ; .. into high byte and carry bit
        LDX #2              ; process 3 bytes
DECDBL  LDA U0AA0,X         ; load current value of byte
        ADC U0AA0,X         ; add it to itself plus the carry bit
        STA U0AA0,X         ; store it back in the same location
        DEX                 ; decrement byte counter
        BPL DECDBL          ; loop until all bytes processed
        DEY                 ; decrement bit counter
        BNE DECML2          ; loop until all bits processed
        PLP                 ; restore processor status
        RTS

; load the input value and fall through to print it
PRINUM  PHA                 ; save accumulator
        LDA TMP0            ; copy input low byte to work buffer
        STA U0AA0+2
        LDA TMP0+1          ; copy input high byte to work buffer
        STA U0AA0+1
        LDA #0              ; clear overflow byte in work buffer
        STA U0AA0
        PLA                 ; restore accumulator

; print number in specified base without leading zeros
NMPRNT  STA DIGCNT          ; number of digits in accumulator
        STY NUMBIT          ; bits per digit passed in Y register
DIGOUT  LDY NUMBIT          ; get bits to process
        LDA #0              ; clear accumulator
ROLBIT  ASL U0AA0+2         ; shift bits out of low byte
        ROL U0AA0+1         ; ... into high byte
        ROL U0AA0           ; ... into overflow byte
        ROL A               ; ... into accumulator
        DEY                 ; decrement bit counter
        BPL ROLBIT          ; loop until all bits processed
        TAY                 ; check whether accumulator is 0
        BNE NZERO           ; if not, print it
        CPX #1              ; have we output the max number of digits?
        BEQ NZERO           ; if not, print it
        LDY DIGCNT          ; how many digits have we output?
        BEQ ZERSUP          ; skip output if digit is 0
NZERO   INC DIGCNT          ; increment digit counter
        ORA #$30            ; add numeric value to ascii '0' to get ascii char
        JSR CHROUT          ; output character
ZERSUP  DEX                 ; decrement number of leading zeros
        BNE DIGOUT          ; next digit
        RTS

; -----------------------------------------------------------------------------
; disk status/command [@]
DSTAT   BNE CHGDEV          ; if device address was given, use it
        LDX #8              ; otherwise, default to 8
        .BYTE $2C           ; absolute BIT opcode consumes next word (LDX TMP0)
CHGDEV  LDX TMP0            ; load device address from parameter
        CPX #4              ; make sure device address is in range 4-31
        BCC IOERR
        CPX #32
        BCS IOERR
        STX TMP0
        LDA #0              ; clear status
        STA SATUS
        STA FNLEN           ; empty filename
        JSR GETCHR          ; get next character
        BEQ INSTAT1         ; null, display status
        DEC CHRPNT          ; back up 1 char
        CMP #"$"            ; $, display directory
        BEQ DIRECT
        LDA TMP0            ; command specified device to listen
        JSR LISTEN
        LDA #$6F            ; secondary address 15 (only low nybble used)
        JSR SECOND

; send command to device
DCOMD   LDX CHRPNT          ; get next character from buffer
        INC CHRPNT
        LDA INBUFF,X
        BEQ INSTAT          ; break out of loop if it's null
        JSR CIOUT           ; otherwise output it to the serial bus
        BCC DCOMD           ; unconditional loop: CIOUT clears carry before RTS

; get device status
INSTAT  JSR UNLSN           ; command device to unlisten
INSTAT1 JSR CRLF            ; new line
        LDA TMP0            ; load device address
        JSR TALK            ; command device to talk
        LDA #$6F            ; secondary address 15 (only low nybble used)
        JSR TKSA
RDSTAT  JSR ACPTR           ; read byte from serial bus
        JSR CHROUT          ; print it
        CMP #$0D            ; if the byte is CR, exit loop
        BEQ DEXIT
        LDA SATUS           ; check status
        AND #$BF            ; ignore EOI bit
        BEQ RDSTAT          ; if no errors, read next byte
DEXIT   JSR UNTLK           ; command device to stop talking
        JMP STRT            ; back to mainloop
IOERR   JMP ERROR           ; handle error

; get directory
DIRECT  LDA TMP0            ; load device address
        JSR LISTEN          ; command device to listen
        LDA #$F0            ; secondary address 0 (only low nybble used)
        JSR SECOND
        LDX CHRPNT          ; get index of next character
DIR2    LDA INBUFF,X        ; get next character from buffer
        BEQ DIR3            ; break if it's null
        JSR CIOUT           ; send character to device
        INX                 ; increment characer index
        BNE DIR2            ; loop if it hasn't wrapped to zero
DIR3    JSR UNLSN           ; command device to unlisten
        JSR CRLF            ; new line
        LDA TMP0            ; load device address
        PHA                 ; save on stack
        JSR TALK            ; command device to talk
        LDA #$60            ; secondary address 0 (only low nybble used)
        JSR TKSA
        LDY #3              ; read 3 16-bit values from device
DIRLIN  STY STORE           ;   ignore the first 2; 3rd is file size
DLINK   JSR ACPTR           ; read low byte from device
        STA TMP0            ; store it
        LDA SATUS           ; check status
        BNE DREXIT          ; exit if error or eof occurred
        JSR ACPTR           ; read high byte from device
        STA TMP0+1          ; store it
        LDA SATUS           ; check status
        BNE DREXIT          ; exit if error or eof cocurred
        DEC STORE           ; decrement byte count
        BNE DLINK           ; loop if bytes remain
        JSR CVTDEC          ; convert last 16-bit value to decimal
        LDA #0              ; clear digit count
        LDX #6              ; max 6 digits
        LDY #3              ; 3 bits per digit
        JSR NMPRNT          ; output number
        LDA #" "            ; output space
        JSR CHROUT
DNAME   JSR ACPTR           ; get a filename character from the device
        BEQ DMORE           ; if it's null, break out of loop
        LDX SATUS           ; check for errors or eof
        BNE DREXIT          ; if found exit early
        JSR CHROUT          ; output character
        CLC
        BCC DNAME           ; unconditional branch to read next char
DMORE   JSR CRLF
        JSR STOP            ; check for stop key
        BEQ DREXIT          ; exit early if pressed
        JSR GETIN           ; pause if a key was pressed
        BEQ NOPAWS
PAWS    JSR GETIN           ; wait until another key is pressed
        BEQ PAWS            
NOPAWS  LDY #2
        BNE DIRLIN          ; unconditional branch to read next file
DREXIT  JSR UNTLK           ; command device to untalk
        PLA                 ; restore accumulator
        JSR LISTEN          ; command device to listen
        LDA #$E0            ; secondary address 0 (only low nybble is used)
        JSR SECOND
        JSR UNLSN           ; command device to unlisten
        JMP STRT            ; back to mainloop

; -----------------------------------------------------------------------------
; print and clear routines
CLINE   JSR CRLF            ; send CR+LF
        JMP SNCLR           ; clear line
SNDCLR  JSR SNDMSG
SNCLR   LDY #$28            ; loop 40 times
SNCLP   LDA #$20            ; output space character
        JSR CHROUT
        LDA #$14            ; output delete character
        JSR CHROUT
        DEY
        BNE SNCLP
        RTS

; -----------------------------------------------------------------------------
; display message from table
SNDMSG  LDA MSGBAS,Y        ; Y contains offset in msg table
        PHP
        AND #$7F            ; strip high bit before output
        JSR CHOUT
        INY
        PLP
        BPL SNDMSG          ; loop until high bit is set
        RTS

; -----------------------------------------------------------------------------
; message table; last character has high bit set
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

; -----------------------------------------------------------------------------
; addressing mode table - nybbles provide index into MODE2 table
; for opcodes XXXXXXY0, use XXXXXX as index into table
; for opcodes WWWXXY01  use $40 + XX as index into table
; use right nybble if Y=0; use left nybble if Y=1

MODE    .BYTE $40,$02,$45,$03   ; even opcodes
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
        .BYTE $62,$13,$78,$A9   ; opcodes ending in 01

; addressing mode format definitions indexed by nybbles from MODE table

; left 6 bits define which characters appear in the assembly operand
; left 3 bits are before the address; next 3 bits are after

; right-most 2 bits define length of binary operand

; index               654 321
; 1st character       $(# ,),  
; 2nd character        $$ X Y    length  format      idx mode
MODE2   .BYTE $00   ; 000 000    00                  0   error
        .BYTE $21   ; 001 000    01      #$00        1   immediate
        .BYTE $81   ; 100 000    01      $00         2   zero-page
        .BYTE $82   ; 100 000    10      $0000       3   absolute
        .BYTE $00   ; 000 000    00                  4   implied
        .BYTE $00   ; 000 000    00                  5   accumulator
        .BYTE $59   ; 010 110    01      ($00,X)     6   indirect,X
        .BYTE $4D   ; 010 011    01      ($00),Y     7   indirect,Y
        .BYTE $91   ; 100 100    01      $00,X       8   zero-page,X
        .BYTE $92   ; 100 100    10      $0000,X     9   absolute,X
        .BYTE $86   ; 100 001    10      $0000,Y     A   absolute,Y
        .BYTE $4A   ; 010 010    10      ($0000)     B   indirect
        .BYTE $85   ; 100 001    01      $00,Y       C   zero-page,Y
        .BYTE $9D   ; 100 111    01      $0000*      D   relative

; * relative is special-cased so format bits don't match


; character lookup tables for the format definitions in MODE2

CHAR1   .BYTE $2C,$29,$2C       ; ","  ")"  ","
        .BYTE $23,$28,$24       ; "#"  "("  "$"

CHAR2   .BYTE $59,$00,$58       ; "Y"   0   "X"
        .BYTE $24,$24,$00       ; "$"  "$"   0

; -----------------------------------------------------------------------------
; 3-letter mnemonics packed into two bytes (5 bits per letter)

        ; left 8 bits
        ; XXXXX000 opcodes
MNEML   .BYTE $1C,$8A,$1C,$23   ; BRK PHP BPL CLC
        .BYTE $5D,$8B,$1B,$A1   ; JSR PLP BMI SEC
        .BYTE $9D,$8A,$1D,$23   ; RTI PHA BVC CLI
        .BYTE $9D,$8B,$1D,$A1   ; RTS PLA BVS SEI
        .BYTE $00,$29,$19,$AE   ; ??? DEY BCC TYA
        .BYTE $69,$A8,$19,$23   ; LDY TAY BCS CLV
        .BYTE $24,$53,$1B,$23   ; CPY INY BNE CLD
        .BYTE $24,$53,$19,$A1   ; CPX INX BEQ SED
        ; XXXYY100 opcodes
        .BYTE $00,$1A,$5B,$5B   ; ??? BIT JMP JMP
        .BYTE $A5,$69,$24,$24   ; STY LDY CPY CPX
        ; 1XXX1010 opcodes
        .BYTE $AE,$AE,$A8,$AD   ; TXA TXS TAX TSX
        .BYTE $29,$00,$7C,$00   ; DEX ??? NOP ???
        ; XXXYYY10 opcodes
        .BYTE $15,$9C,$6D,$9C   ; ASL ROL LSR ROR
        .BYTE $A5,$69,$29,$53   ; STX LDX DEC INC
        ; XXXYYY01 opcodes
        .BYTE $84,$13,$34,$11   ; ORA AND EOR ADC
        .BYTE $A5,$69,$23,$A0   ; STA LDA CMP SBC

        ; right 7 bits, left justified
        ; XXXXX000 opcodes
MNEMR   .BYTE $D8,$62,$5A,$48   ; BRK PHP BPL CLC
        .BYTE $26,$62,$94,$88   ; JSR PLP BMI SEC
        .BYTE $54,$44,$C8,$54   ; RTI PHA BVC CLI
        .BYTE $68,$44,$E8,$94   ; RTS PLA BVS SEI
        .BYTE $00,$B4,$08,$84   ; ??? DEY BCC TYA
        .BYTE $74,$B4,$28,$6E   ; LDY TAY BCS CLV
        .BYTE $74,$F4,$CC,$4A   ; CPY INY BNE CLD
        .BYTE $72,$F2,$A4,$8A   ; CPX INX BEQ SED
        ; XXXYY100 opcodes
        .BYTE $00,$AA,$A2,$A2   ; ??? BIT JMP JMP
        .BYTE $74,$74,$74,$72   ; STY LDY CPY CPX
        ; 1XXX1010 opcodes
        .BYTE $44,$68,$B2,$32   ; TXA TXS TAX TSX
        .BYTE $B2,$00,$22,$00   ; DEX ??? NOP ???
        ; XXXYYY10 opcodes
        .BYTE $1A,$1A,$26,$26   ; ASL ROL LSR ROR
        .BYTE $72,$72,$88,$C8   ; STX LDX DEC INC
        ; XXXYYY01 opcodes
        .BYTE $C4,$CA,$26,$48   ; ORA AND EOR ADC
        .BYTE $44,$44,$A2,$C8   ; STA LDA CMP SBC
        .BYTE $0D,$20,$20,$20

; -----------------------------------------------------------------------------
; single-character commands
KEYW    .TEXT "ACDFGHJMRTX@.>;"
HIKEY   .TEXT "$+&%LSV"
KEYTOP  =*

; vectors corresponding to commands above
KADDR   .WORD ASSEM-1,COMPAR-1,DISASS-1,FILL-1
        .WORD GOTO-1,HUNT-1,JSUB-1,DSPLYM-1
        .WORD DSPLYR-1,TRANS-1,EXIT-1,DSTAT-1
        .WORD ASSEM-1,ALTM-1,ALTR-1

; -----------------------------------------------------------------------------
MODTAB  .BYTE $10,$0A,$08,02    ; modulo number systems
LENTAB  .BYTE $04,$03,$03,$01   ; bits per digit

LINKAD  .WORD BREAK             ; address of brk handler
SUPAD   .WORD SUPER             ; address of entry point
