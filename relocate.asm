
; Relocatable code stub for Supermon 64 by Jim Butterfield
; This code was disassembled from the original Supermon+64 V1.2 binary.

; The relocation stub starts at the Start of Variables pointer (VARTAB) and
; works backwards through the Supermon64 machine code, copying it to the top
; of basic memory pointer (MEMSIZ), and decrementing the pointer as it goes.
; Relative addresses that need to be adjusted are marked with a $36 byte
; immediately following them. Since we're working backwards, the marker is
; encountered first, then the high byte, then the low byte of the address
; needing to be adjusted. The relative addresses are calculated such that
; adding the top of memory to them will yield the absolute address of the
; jump target in the relocated code.

; build.py will build a relocatable Supermon64 binary from this stub plus
; standard Supermon64 binaries assembled to two different pages.

; ----------------------------------------------------------------------------
; variables

SOURCE  = $22               ; first temp variable
TOPMEM  = $24               ; highest address available to BASIC
LASTBYT = $26               ; previous byte encountered
VARTAB  = $2D               ; pointer to start of BASIC variable storage area
FRETOP  = $33               ; pointer to bottom of string text storage area
TARGET  = $37               ; end of basic memory/start of machine code (aka MEMSIZ)


; ----------------------------------------------------------------------------
; basic header

        * = $0801

        ; 100 PRINT "{DOWN}SUPERMON+64    JIM BUTTERFIELD"
        ; 110 SYS(PEEK(43)+256*PEEK(44)+71)

        .BYTE $29,$08,$64,$00,$99,$20,$22,$11
        .TEXT "SUPERMON+64    JIM BUTTERFIELD"
        .BYTE $22,$00,$43,$08,$6E,$00,$9E,$28
        .BYTE $C2,$28,$34,$33,$29,$AA,$32,$35
        .BYTE $36,$AC,$C2,$28,$34,$34,$29,$AA
        .BYTE $37,$31,$29,$00,$00,$00,$00,$00
        .BYTE $00

; ----------------------------------------------------------------------------
; relocator stub

        LDA VARTAB          ; start copying from the start of basic variables
        STA SOURCE
        LDA VARTAB+1
        STA SOURCE+1
        LDA TARGET          ; start copying to the end of BASIC memory
        STA TOPMEM
        LDA TARGET+1
        STA TOPMEM+1
LOOP    LDY #$00            ; no offset from pointers
        LDA SOURCE          ; decrement two-byte source address
        BNE NB1
        DEC SOURCE+1
NB1     DEC SOURCE
        LDA (SOURCE),Y      ; get byte currently pointed to by SOURCE
        CMP #$36            ; check for address marker ($36)
        BNE NOADJ           ; skip address adjustment unless found
        LDA SOURCE          ; decrement two-byte source address
        BNE NB2
        DEC SOURCE+1
NB2     DEC SOURCE
        LDA (SOURCE),Y      ; get byte currently pointed to by SOURCE
        CMP #$36            ; check for second consecutive marker ($36)
        BEQ DONE            ; if found, we're done with relocation
        STA LASTBYT         ; if not, save byte for later
        LDA SOURCE          ; decrement two-byte source address
        BNE NB3
        DEC SOURCE+1
NB3     DEC SOURCE
        LDA (SOURCE),Y      ; current byte is low byte of relative address
        CLC 
        ADC TOPMEM          ; calc absolute low byte by adding top of memory
        TAX                 ; save absolute low byte in X
        LDA LASTBYT         ; previous byte is high byte of relative address
        ADC TOPMEM+1        ; calc absolute high byte by adding top of memory
        PHA                 ; save absolute high byte on stack
        LDA TARGET          ; decrement two-byte target address
        BNE NB4
        DEC TARGET+1
NB4     DEC TARGET
        PLA                 ; retrieve absolute high byte from stack
        STA (TARGET),Y      ; save it to the target address
        TXA                 ; retrieve absolute low byte from stack
NOADJ   PHA                 ; save current byte on stack
        LDA TARGET          ; decrement two-byte target address
        BNE NB5
        DEC TARGET+1
NB5     DEC TARGET
        PLA                 ; retrieve current byte from stack
        STA (TARGET),Y      ; save it in the target address
        CLC                 ; clear carry for unconditional loop
        BCC LOOP            ; rinse, repeat
DONE    LDA TARGET          ; fix pointer to string storage
        STA FRETOP
        LDA TARGET+1
        STA FRETOP+1
        JMP (TARGET)        ; jump to the beginning of the relocated code
