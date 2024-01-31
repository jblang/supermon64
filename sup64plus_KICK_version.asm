// Kick Assembler version by mikroman (m.j. loewen) Jan,2024.
// This version builds identical to the TASS version which was
// converted by j.b. langston in 2016/17, which was 
// converted from Jim Butterfield's supermon.src which was.....
// you get the picture.
//
// ********************************
// * supermon+ 64 jim butterfield *
// * v1.2   august 20 1985        *
// * SYS 38169 $9519
// ********************************

// reformatted and annotated in late 2016/early 2017 by j.b. langston.
// 
// i've made the minimum necessary changes to this code to get it to assemble
// with 64tass.  specifically, i changed the following directives from pal
// that 64tass doesn't support:
//   - .asc => .text
//   - *=*+x => .fill x
// aside from this, i have adopted a strict whitespace and comments only
// policy so that i preserve code exactly as jim butterfield wrote it.
// 
// i think my comments are correct but i don't guarantee i haven't made
// any errors. sadly jim isn't around to ask anymore. if you spot any
// misunderstanings or errors in my comments, please report them.
//                  j.b. langston.

// ------------------------------------BEGIN------------------------------------ //

.encoding "petscii_mixed"

// -----------------------------------------------------------------------------
// temporary pointers

.label  tmp0 = $c1          // used to return input, often holds end address
.label  tmp2 = $c3          // usually holds start address

// -----------------------------------------------------------------------------
// kernal variables

.label  satus   = $90       // kernal i/o status word
.label  fnlen   = $b7       // length of current filename
.label  sadd    = $b9       // current secondary address (official name sa)
.label  fa      = $ba       // current device number
.label  fnadr   = $bb       // pointer to current filename
.label  ndx     = $c6       // number of characters in keyboard buffer
.label  keyd    = $0277     // keyboard buffer
.label  bkvec   = $0316     // brk instruction vector (official name cbinv)

// variables

* = $0100 virtual           // store variables in tape error buffer

acmd:    .byte $00          // addressing command
length:  .byte $00          // length of operand
mnemw:   .fill 3,0          // 3 letter mnemonic buffer
savx:    .byte $00          // 1 byte temp storage, often to save x register
opcode:  .byte $00          // current opcode for assembler/disassembler
upflg:   .byte $00          // flag: count up (bit 7 clear) or down (bit 7 set)
digcnt:  .byte $00          // digit count
indig:   .byte $00          // numeric value of single digit
numbit:  .byte $00          // numeric base of input
stash:   .byte $00,$00      // 2-byte temp storage
u0aa0:   .fill 10,0         // work buffer
u0aae:                      // end of work buffer
stage:   .fill 30,0         // staging buffer for filename, search, etc.
estage:                     // end of staging buffer

* = $0200 virtual           // store more variables in basic line editor buffer

inbuff:  .fill 40,0         // 40-character input buffer
endin:                      // end of input buffer

// the next 7 locations are used to store the registers when
// entering the monitor and restore them when exiting.

pch:     .byte $00           // program counter high byte
pcl:     .byte $00           // program counter low byte
sr:      .byte $00           // status register
acc:     .byte $00           // accumulator
xr:      .byte $00           // x register
yr:      .byte $00           // y register
sp:      .byte $00           // stack pointer

store:   .fill 2,0          // 2-byte temp storage
chrpnt:  .byte $00          // current position in input buffer
savy:    .byte $00          // temp storage, often to save y register
u9f:     .byte $00          // index into assembler work buffer

// -----------------------------------------------------------------------------
// kernal entry points

.const setmsg  = $ff90      // set kernel message control flag
.const second  = $ff93      // set secondary address after listen
.const tksa    = $ff96      // send secondary address after talk
.const listen  = $ffb1      // command serial bus device to listen
.const talk    = $ffb4      // command serial bus device to talk
.const setlfs  = $ffba      // set logical file parameters
.const setnam  = $ffbd      // set filename
.const acptr   = $ffa5      // input byte from serial bus
.const ciout   = $ffa8      // output byte to serial bus
.const untlk   = $ffab      // command serial bus device to untalk
.const unlsn   = $ffae      // command serial bus device to unlisten
.const chkin   = $ffc6      // define input channel
.const clrchn  = $ffcc      // restore default devices
.const input   = $ffcf      // input a character (official name chrin)
.const chrout  = $ffd2      // output a character
.const load    = $ffd5      // load from device
.const save    = $ffd8      // save to device
.const stop    = $ffe1      // check the stop key
.const getin   = $ffe4      // get a character

// -----------------------------------------------------------------------------
// set up origin

* = $9519 "disassembly"

// -----------------------------------------------------------------------------
// initial entry point

super:
        ldy #msg4-msgbas    // display "..sys "
        jsr sndmsg
        lda supad           // store entry point address in tmp0
        sta tmp0
        lda supad+1
        sta tmp0+1
        jsr cvtdec          // convert address to decimal
        lda #0
        ldx #6
        ldy #3
        jsr nmprnt          // print entry point address
        jsr crlf
        lda linkad          // set brk vector
        sta bkvec
        lda linkad+1
        sta bkvec+1
        lda #$80            // disable kernel control messages
        jsr setmsg          // and enable error messages
        brk

// -----------------------------------------------------------------------------
// brk handler

break:
        ldx #$05            // pull registers off the stack
bstack:
        pla                 // order: y,x,a,sr,pcl,pch
        sta pch,x           // store in memory
        dex 
        bpl bstack
        cld                 // disable bcd mode
        tsx                 // store stack pointer in memory 
        stx sp
        cli                 // enable interupts

// -----------------------------------------------------------------------------
// display registers [r]

dsplyr:
        ldy #msg2-msgbas    // display headers
        jsr sndclr
        lda #$3b            // prefix registers with ";" to allow editing
        jsr chrout
        lda #$20
        jsr chrout
        lda pch             // print 2-byte program counter
        jsr wrtwo
        ldy #1              // start 1 byte after pc high byte
disj:
        lda pch,y           // loop through rest of the registers
        jsr wrbyte          // print 1-byte register value
        iny 
        cpy #7              // there are a total of 5 registers to print
        bcc disj

// -----------------------------------------------------------------------------
// main loop

strt:
        jsr crlf            // new line
        ldx #0              // point at start of input buffer
        stx chrpnt 
smove:
        jsr input           // chrin kernal call to input a character
        sta inbuff,x        // store in input buffer
        inx 
        cpx #endin-inbuff   // error if buffer is full
        bcs error
        cmp #$0d            // keep reading until cr
        bne smove
        lda #0              // null-terminate input buffer
        sta inbuff-1,x      // (replacing the cr)
st1:
        jsr getchr          // get a character from the buffer
        beq strt            // start over if buffer is empty
        cmp #$20            // skip leading spaces
        beq st1
s0:
        ldx #keytop-keyw    // loop through valid command characters
s1:
        cmp keyw,x          // see if input character matches
        beq s2              // command matched, dispatch it
        dex                 // no match, check next command
        bpl s1              // keep trying until we've checked them all
                            // then fall through to error handler

// -----------------------------------------------------------------------------
// handle error

error:
        ldy #msg3-msgbas    // display "?" to indicate error and go to new line
        jsr sndmsg
        jmp strt            // back to main loop

// -----------------------------------------------------------------------------
// dispatch command

s2:
        cpx #$13            // last 3 commands in table are load/save/validate
        bcs lsv             //   which are handled by the same subroutine
        cpx #$0f            // next 4 commands are base conversions
        bcs cnvlnk          //   which are handled by the same subroutine
        txa                 // remaining commands dispatch through vector table
        asl                 // multiply index of command by 2
        tax                 //   since table contains 2-byte addresses
        lda kaddr+1,x       // push address from vector table onto stack
        pha                 //   so that the rts from getpar will jump there
        lda kaddr,x
        pha
        jmp getpar          // get the first parameter for the command
lsv:
        sta savy            // handle load/save/validate
        jmp ld
cnvlnk:
        jmp convrt          // handle base conversion

// -----------------------------------------------------------------------------
// exit monitor [x]

exit:
        jmp ($a002)         // jump to warm-start vector to reinitialize basic

// -----------------------------------------------------------------------------
// display memory [m]

dsplym:
        bcs dspm11          // start from previous end addr if no address given
        jsr copy12          // save start address in tmp2
        jsr getpar          // get end address in tmp0
        bcc dsmnew          // did user specify one?
dspm11:
        lda #$0b            // if not, show 12 lines by default
        sta tmp0
        bne dspbyt          // always true, but bne uses 1 byte less than jmp
dsmnew:
        jsr sub12           // end addr given, calc bytes between start and end
        bcc merror          // error if start is after end
        ldx #3              // divide by 8 (shift right 3 times)
dspm01:
        lsr tmp0+1
        ror tmp0
        dex 
        bne dspm01
dspbyt:
        jsr stop            // check for stop key
        beq dspmx           // exit early if pressed
        jsr dispmem         // display 1 line containing 8 bytes
        lda #8              // increase start address by 8 bytes
        jsr bumpad2
        jsr suba1           // decrement line counter
        bcs dspbyt          // show another line until it's < 0
dspmx:
        jmp strt            // back to main loop
merror:
        jmp error           // handle error

// -----------------------------------------------------------------------------
// alter registers [;]

altr:
        jsr copy1p          // store first parameter in pc
        ldy #0              // init counter
altr1:
        jsr getpar          // get value for next register
        bcs altrx           // exit early if no more values given
        lda tmp0            // store in memory, offset from sr
        sta sr,y            // these locations will be transferred to the
        iny                 //   actual registers before exiting the monitor
        cpy #$05            // have we updated all 5 yet?
        bcc altr1           // if not, get next
altrx:
        jmp strt            // back to main loop

// -----------------------------------------------------------------------------
// alter memory [>]

altm:
        bcs altmx           // exit if no parameter provided
        jsr copy12          // copy parameter to start address
        ldy #0
altm1:
        jsr getpar          // get value for next byte of memory
        bcs altmx           // if none given, exit early
        lda tmp0            // poke value into memory at start address + y
        sta (tmp2),y
        iny                 // next byte
        cpy #8              // have we read 8 bytes yet?
        bcc altm1           // if not, read the next one
altmx:
        lda #$91            // move cursor up
        jsr chrout
        jsr dispmem         // re-display line to make ascii match hex
        jmp strt            // back to main loop

// -----------------------------------------------------------------------------
// goto (run) [g]

goto:
        ldx sp              // load stack pointer from memory
        txs                 // save in sp register
goto2:
        jsr copy1p          // copy provided address to pc
        sei                 // disable interrupts
        lda pch             // push pc high byte on stack
        pha
        lda pcl             // push pc low byte on stack
        pha
        lda sr              // push status byte on stack
        pha
        lda acc             // load accumulator from memory
        ldx xr              // load x from memory
        ldy yr              // load y from memory
        rti                 // return from interrupt (pops pc and sr)

// jump to subroutine [j]

jsub:
        ldx sp              // load stack pointer from memory
        txs                 // save value in sp register
        jsr goto2           // same as goto command
        sty yr              // save y to memory
        stx xr              // save x to memory
        sta acc             // save accumulator to memory
        php                 // push processor status on stack
        pla                 // pull processor status into a
        sta sr              // save processor status to memory
        jmp dsplyr          // display registers

// -----------------------------------------------------------------------------
// display 8 bytes of memory

dispmem:
        jsr crlf            // new line
        lda #'>'            // prefix > so memory can be edited in place
        jsr chrout
        jsr showad          // show address of first byte on line
        ldy #0
        beq dmemgo          // showad already printed a space after the address
dmemlp:
        jsr space           // print space between bytes
dmemgo:
        lda (tmp2),y        // load byte from start address + y
        jsr wrtwo           // output hex digits for byte
        iny                 // next byte
        cpy #8              // have we output 8 bytes yet?
        bcc dmemlp          // if not, output next byte
        ldy #msg5-msgbas    // if so, output : and turn on reverse video
        jsr sndmsg          //   before displaying ascii representation
        ldy #0              // back to first byte in line
dchar:
        lda (tmp2),y        // load byte at start address + y
        tax                 // stash in x
        and #$bf            // clear 6th bit
        cmp #$22            // is it a quote (")?
        beq ddot            // if so, print . instead
        txa                 // if not, restore character
        and #$7f            // clear top bit
        cmp #$20            // is it a printable character (>= $20)?
        txa                 // restore character
        bcs dchrok          // if printable, output character
ddot:
        lda #$2e            // if not, output '.' instaed
dchrok:
        jsr chrout
        iny                 // next byte
        cpy #8              // have we output 8 bytes yet?
        bcc dchar           // if not, output next byte
        rts 

// -----------------------------------------------------------------------------
// compare memory [c]

compar:
        lda #0              // bit 7 clear signals compare
        .byte $2c           // absolute bit opcode consumes next word (lda #$80)

// transfer memory [t]

trans:
        lda #$80            // bit 7 set signals transfer
        sta savy            // save compare/transfer flag in savy
        lda #0              // assume we're counting up (bit 7 clear)
        sta upflg           // save direction flag
        jsr getdif          // get two addresses and calculate difference
                            //   tmp2 = source start
                            //   stash = source end
                            //   store = length
        bcs terror          // carry set indicates error
        jsr getpar          // get destination address in tmp0
        bcc tokay           // carry set indicates error
terror:
        jmp error           // handle error
tokay:
        bit savy            // transfer or compare?
        bpl compar1         // high bit clear indicates compare
        lda tmp2            // if it's a transfer, we must take steps
        cmp tmp0            //   to avoid overwriting the source bytes before 
        lda tmp2+1          //   they have been transferred
        sbc tmp0+1          // compare source (tmp2) to destination (tmp0)
        bcs compar1         // and count up if source is before than desitnation
        lda store           // otherwise, start at end and count down...
        adc tmp0            // add length (store) to desintation (tmp0)
        sta tmp0            // to calculate end of destination
        lda store+1
        adc tmp0+1
        sta tmp0+1
        ldx #1              // change source pointer from beginning to end
tdown:
        lda stash,x         // tmp2 = source end (stash)
        sta tmp2,x
        dex 
        bpl tdown
        lda #$80            // high bit set in upflg means count down
        sta upflg
compar1:
        jsr crlf            // new line
        ldy #0              // no offset from pointer
tcloop:
        jsr stop            // check for stop key
        beq texit           // exit if pressed
        lda (tmp2),y        // load byte from source
        bit savy            // transfer or compare?
        bpl compar2         // skip store if comparing
        sta (tmp0),y        // otherwise, store in destination
compar2:
        cmp (tmp0),y        // compare to destination
        beq tmvad           // don't show address if equal
        jsr showad          // show address
tmvad:
        bit upflg           // counting up or down?
        bmi tdecad          // high bit set means we're counting down
        inc tmp0            // increment destination low byte
        bne tincok
        inc tmp0+1          // carry to high byte if necessary
        bne tincok
        jmp error           // error if high byte overflowed
tdecad:
        jsr suba1           // decrement destination (tmp0)
        jsr sub21           // decrement source (tmp2)
        jmp tmor
tincok:
        jsr adda2           // increment source (tmp2)
tmor:
        jsr sub13           // decrement length
        bcs tcloop          // loop until length is 0
texit:
        jmp strt            // back to main loop

// -----------------------------------------------------------------------------
// hunt memory [h]

hunt:
        jsr getdif          // get start (tmp2) and end (tmp0) of haystack
        bcs herror          // carry indicates error
        ldy #0
        jsr getchr          // get a single character
        cmp #'''            // is it a single quote?
        bne nostrh          // if not, input needle as hex bytes
        jsr getchr          // if so, input needle as string
        cmp #0
        beq herror          // error if needle isn't at least one byte
hpar:
        sta stage,y         // save char in staging area
        iny 
        jsr getchr          // get another char
        beq htgo            // if it's null start searching
        cpy #estage-stage   // have we filled up the needle staging area?
        bne hpar            // if not, get another character
        beq htgo            // if so, start searching
nostrh:
        jsr rdpar           // read hex bytes if string not indicated
hlp:
        lda tmp0            // save last read byte in staging area
        sta stage,y
        iny                 // get another hex byte
        jsr getpar
        bcs htgo            // if there is none, start searching
        cpy #estage-stage   // have we filled up the needle staging area?
        bne hlp             // if not, get another byte
htgo:
        sty savy            // save length of needle
        jsr crlf            // new line
hscan:
        ldy #0
hlp3:
        lda (tmp2),y        // get first byte in haystack
        cmp stage,y         // compare it to first byte of needle
        bne hnoft           // if it doesn't match, we haven't found anything
        iny                 // if it does, check the next byte
        cpy savy            // have we reached the end of the needle?
        bne hlp3            // if not, keep comparing bytes
        jsr showad          // match found, show address
hnoft:
        jsr stop            // no match, check for stop key
        beq hexit           // exit prematurely if pressed
        jsr adda2           // increment haystack pointer
        jsr sub13           // decrement haystack length
        bcs hscan           // still more haystack? keep searching
hexit:
        jmp strt            // back to main loop
herror:
        jmp error           // handle error

// -----------------------------------------------------------------------------
// load, save, or verify [lsv]

ld:
        ldy #1              // default to reading from tape, device #1
        sty fa
        sty sadd            // default to secondary address #1
        dey
        sty fnlen           // start with an empty filename
        sty satus           // clear status
        lda #>stage         // set filename pointer to staging buffer
        sta fnadr+1
        lda #<stage
        sta fnadr
l1:
        jsr getchr          // get a character
        beq lshort          // no filename given, try load or verify from tape
        cmp #$20            // skip leading spaces
        beq l1
        cmp #$22            // error if filename doesn't start with a quote
        bne lerror
        ldx chrpnt          // load current char pointer into index reg
l3:
        lda inbuff,x        // load current char from buffer to accumulator
        beq lshort          // no filename given, try load or verify from tape
        inx                 // next char
        cmp #$22            // is it a quote?
        beq l8              // if so, we've reached the end of the filename
        sta (fnadr),y       // if not, save character in filename buffer
        inc fnlen           // increment filename length
        iny 
        cpy #estage-stage   // check whether buffer is full
        bcc l3              // if not, get another character
lerror:
        jmp error           // if so, handle error
l8:
        stx chrpnt          // set character pointer to the current index
        jsr getchr          // eat separator between filename and device #
        beq lshort          // no separator, try to load or verify from tape
        jsr getpar          // get device number
        bcs lshort          // no device # given, try load or verify from tape
        lda tmp0            // set device number for kernal routines
        sta fa
        jsr getpar          // get start address for load or save in tmp0
        bcs lshort          // no start address, try to load or verify
        jsr copy12          // transfer start address to tmp2
        jsr getpar          // get end address for save in tmp0
        bcs ldaddr          // no end address, try to load to given start addr
        jsr crlf            // new line
        ldx tmp0            // put low byte of end address in x
        ldy tmp0+1          // put high byte of end address in y
        lda savy            // confirm that we're doing a save
        cmp #'s'
        bne lerror          // if not, error due to too many params
        lda #0
        sta sadd            // set secondary address to 0
        lda #tmp2           // put addr of zero-page pointer to data in a
        jsr save            // call kernal save routine
lsvxit:
        jmp strt            // back to mainloop
lshort:
        lda savy            // check which command we received
        cmp #'v'
        beq loadit          // we're doing a verify so don't set a to 0
        cmp #'l'
        bne lerror          // error due to not enough params for save
        lda #0              // 0 in a signals load, anything else is verify
loadit:
        jsr load            // call kernal load routine
        lda satus           // get i/o status
        and #$10            // check bit 5 for checksum error
        beq lsvxit          // if no error go back to mainloop
        lda savy            // ?? not sure what these two lines are for...
        beq lerror          // ?? savy will never be 0, so why check?
        ldy #msg6-msgbas    // display "error" if checksum didn't match
        jsr sndmsg
        jmp strt            // back to mainloop
ldaddr:
        ldx tmp2            // load address low byte in x
        ldy tmp2+1          // load address high byte in y
        lda #0              // 0 in a signals load
        sta sadd            // secondary addr 0 means load to addr in x and y
        beq lshort          // execute load

// -----------------------------------------------------------------------------
// fill memory [f]

fill:
        jsr getdif          // start in tmp2, end in stash, length in store
        bcs aerror          // carry set indicates error
        jsr getpar          // get value to fill in tmp0
        bcs aerror          // carry set indicates error
        jsr getchr          // any more characters triggers an error
        bne aerror
        ldy #0              // no offset
fillp:
        lda tmp0            // load value to fill in accumulator
        sta (tmp2),y        // store fill value in current address
        jsr stop            // check for stop key
        beq fstart          // if pressed, back to main loop
        jsr adda2           // increment address
        jsr sub13           // decrement length
        bcs fillp           // keep going until length reaches 0
fstart:
        jmp strt            // back to main loop

// -----------------------------------------------------------------------------
// assemble [a.]

// read in mnemonic

assem:
        bcs aerror          // error if no address given
        jsr copy12          // copy address to tmp2
aget1:
        ldx #0
        stx u0aa0+1         // clear byte that mnemonic gets shifted into
        stx digcnt          // clear digit count
aget2:
        jsr getchr          // get a char
        bne almor           // proceed if the character isn't null
        cpx #0              // it's null, have read a mnemonic yet?
        beq fstart          // if not, silently go back to main loop
almor:
        cmp #$20            // skip leading spaces
        beq aget1
        sta mnemw,x         // put character in mnemonic buffer
        inx
        cpx #3              // have we read 3 characters yet?
        bne aget2           // if not, get next character

// compress mnemonic into two bytes

asqeez:
        dex                 // move to previous char
        bmi aoprnd          // if we're done with mnemonic, look for operand
        lda mnemw,x         // get current character
        sec                 // pack 3-letter mnemonic into 2 bytes (15 bits)
        sbc #$3f            // subtract $3f from ascii code so a-z = 2 to 27
        ldy #$05            // letters now fit in 5 bits// shift them out
ashift:
        lsr                 //   into the first two bytes of the inst buffer
        ror u0aa0+1         // catch the low bit from accumulator in right byte
        ror u0aa0           // catch the low bit from right byte in left byte
        dey                 // count down bits
        bne ashift          // keep looping until we reach zero
        beq asqeez          // unconditional branch to handle next char
aerror:
        jmp error           // handle error

// parse operand

aoprnd:
        ldx #2              // mnemonic is in first two bytes so start at third
ascan:
        lda digcnt          // did we find address digits last time?
        bne aform1          // if so, look for mode chars
        jsr rdval           // otherwise, look for an address
        beq aform0          // we didn't find an address, look for characters
        bcs aerror          // carry flag indicates error
        lda #'$'
        sta u0aa0,x         // prefix addresses with $
        inx                 // next position in buffer
        ldy #4              // non-zero page addresses are 4 hex digits
        lda numbit          // check numeric base in which address was given
        cmp #8              // for addresses given in octal or binary
        bcc aaddr           //   use only the high byte to determine page
        cpy digcnt          // for decimal or hex, force non-zero page addressing
        beq afill0          //   if address was given with four digits or more 
aaddr:
        lda tmp0+1          // check whether high byte of address is zero
        bne afill0          // non-zero high byte means we're not in zero page
        ldy #2              // if it's in zero page, addr is 2 hex digits
afill0:
        lda #$30            // use 0 as placeholder for each hex digit in addr
afil0l:
        sta u0aa0,x         // put placeholder in assembly buffer
        inx                 // move to next byte in buffer
        dey                 // decrement number of remaining digits
        bne afil0l          // loop until all digits have been placed
aform0:
        dec chrpnt          // non-numeric input// back 1 char to see what it was
aform1:
        jsr getchr          // get next character
        beq aescan          // if there is none, we're finished scanning
        cmp #$20            // skip spaces
        beq ascan
        sta u0aa0,x         // store character in assembly buffer
        inx                 // move to next byte in buffer
        cpx #u0aae-u0aa0    // is instruction buffer full?
        bcc ascan           // if not, keep scanning
        bcs aerror          // error if buffer is full

// find matching opcode

aescan:
        stx store           // save number of bytes in assembly buffer
        ldx #0              // start at opcode $00 and check every one until
        stx opcode          //   we find one that matches our criteria
atryop:
        ldx #0
        stx u9f             // reset index into work buffer
        lda opcode
        jsr instxx          // look up instruction format for current opcode
        ldx acmd            // save addressing command for later
        stx store+1
        tax                 // use current opcode as index
        lda mnemr,x         // check right byte of compressed mnemonic
        jsr chekop
        lda mneml,x         // check left byte of compressed mnemonic
        jsr chekop
        ldx #6              // 6 possible characters to check against operand
tryit:
        cpx #3              // are we on character 3?
        bne trymod          // if not, check operand characters
        ldy length          // otherwise, check number of bytes in operand
        beq trymod          // if zero, check operand characters
tryad:
        lda acmd            // otherwise, look for an address
        cmp #$e8            // special case for relative addressing mode
                            //   since it's specified with 4 digits in assembly
                            //   but encoded with only 1 byte in object code
        lda #$30            // '0' is the digit placeholder we're looking for
        bcs try4b           // acmd >= $e8 indicates relative addressing
        jsr chek2b          // acmd < $e8 indicates normal addressing
        dey                 // consume byte
        bne tryad           // check for 2 more digits if not zero-page
trymod:
        asl acmd            // shift a bit out of the addressing command
        bcc ub4df           // if it's zero, skip checking current character
        lda char1-1,x
        jsr chekop          // otherwise first character against operand
        lda char2-1,x       // get second character to check
        beq ub4df           // if it's zero, skip checking it
        jsr chekop          // otherwise check it against hte operand
ub4df:
        dex                 // move to next character
        bne tryit           // repeat tests
        beq trybran
try4b:
        jsr chek2b          // check for 4 digit address placeholder
        jsr chek2b          //   by checking for 2 digits twice
trybran:
        lda store           // get number of bytes in assembly buffer
        cmp u9f             // more bytes left to check?
        beq abran           // if not, we've found a match// build instruction
        jmp bumpop          // if so, this opcode doesn't match// try the next

// convert branches to relative address

abran:
        ldy length          // get number of bytes in operand
        beq a1byte          // if none, just output the opcode
        lda store+1         // otherwise check the address format
        cmp #$9d            // is it a relative branch?
        bne objput          // if not, skip relative branch calculation
        lda tmp0            // calculate the difference between the current
        sbc tmp2            //   address and the branch target (low byte)
        tax                 // save it in x
        lda tmp0+1          // borrow from the high byte if necessary
        sbc tmp2+1
        bcc abback          // if result is negative, we're branching back
        bne serror          // high bytes must be equal when branching forward
        cpx #$82            // difference between low bytes must be < 130
        bcs serror          // error if the address is too far away
        bcc abranx
abback:
        tay                 // when branching backward high byte of target must
        iny                 //   be 1 less than high byte of current address
        bne serror          // if not, it's too far away
        cpx #$82            // difference between low bytes must be < 130
        bcc serror          // if not, it's too far away
abranx:
        dex                 // adjust branch target relative to the 
        dex                 //   instruction following this one
        txa
        ldy length          // load length of operand
        bne objp2           // don't use the absolute address

// assemble machine code

objput:
        lda tmp0-1,y        // get the operand
objp2:
        sta (tmp2),y        // store it after the opcode
        dey
        bne objput          // copy the other byte of operand if there is one
a1byte:
        lda opcode          // put opcode into instruction
        sta (tmp2),y
        jsr crlf            // carriage return
        lda #$91            // back up one line
        jsr chrout
        ldy #msg7-msgbas    // "a " prefix
        jsr sndclr          // clear line
        jsr dislin          // disassemble the instruction we just assembled
        inc length          // instruction length = operand length + 1 byte
        lda length          //   for the opcode
        jsr bumpad2         // increment address by length of instruction
        lda #'a'            // stuff keyboard buffer with next assemble command:
        sta keyd            //   "a xxxx " where xxxx is the next address
        lda #' '            //   after the previously assembled instruction
        sta keyd+1
        sta keyd+6
        lda tmp2+1          // convert high byte of next address to hex
        jsr asctwo
        sta keyd+2          // put it in the keyboard buffer
        stx keyd+3
        lda tmp2            // convert low byte of next address to hex
        jsr asctwo
        sta keyd+4          // put it in the keyboard buffer
        stx keyd+5
        lda #7              // set number of chars in keyboard buffer
        sta ndx
        jmp strt            // back to main loop
serror:
        jmp error           // handle error

// check characters in operand

chek2b:
        jsr chekop          // check two bytes against value in accumulator
chekop:
        stx savx            // stash x
        ldx u9f             // get current index into work buffer
        cmp u0aa0,x         // check whether this opcode matches the buffer
        beq opok            //   matching so far, check the next criteria
        pla                 // didn't match, so throw away return address
        pla                 //   on the stack because we're starting over
bumpop:
        inc opcode          // check the next opcode
        beq serror          // error if we tried every opcode and none fit
        jmp atryop          // start over with new opcode
opok:
        inc u9f             // opcode matches so far// check the next criteria
        ldx savx            // restore x
        rts

// -----------------------------------------------------------------------------
// disassemble [d]

disass:
        bcs dis0ad          // if no address was given, start from last address
        jsr copy12          // copy start address to tmp2
        jsr getpar          // get end address in tmp0
        bcc dis2ad          // if one was given, skip default
dis0ad:
        lda #$14            // disassemble 14 bytes by default
        sta tmp0            // store length in tmp0
        bne disgo           // skip length calculation
dis2ad:
        jsr sub12           // calculate number of bytes between start and end
        bcc derror          // error if end address is before start address
disgo:
        jsr cline           // clear the current line
        jsr stop            // check for stop key
        beq disexit         // exit early if pressed
        jsr dsout1          // output disassembly prefix ". "
        inc length
        lda length          // add length of last instruction to start address
        jsr bumpad2
        lda length          // subtract length of last inst from end address
        jsr suba2
        bcs disgo
disexit:
        jmp strt            // back to mainloop
derror:
        jmp error

dsout1:
        lda #'.'            // output ". " prefix to allow edit and reassemble
        jsr chrout
        jsr space

dislin:
        jsr showad          // show the address of the instruction
        jsr space           // insert a space
        ldy #0              // no offset
        lda (tmp2),y        // load operand of current instruction
        jsr instxx          // get mnemonic and addressing mode for opcode
        pha                 // save index into mnemonic table
        ldx length          // get length of operand
        inx                 // add 1 byte for opcode
dsbyt:
        dex                 // decrement index
        bpl dshex           // show hex for byte being disassembled
        sty savy            // save index
        ldy #msg8-msgbas    // skip 3 spaces
        jsr sndmsg
        ldy savy            // restore index
        jmp nxbyt
dshex:
        lda (tmp2),y        // show hex for byte
        jsr wrbyte

nxbyt:
        iny                 // next byte
        cpy #3              // have we output 3 bytes yet?
        bcc dsbyt           // if not, loop
        pla                 // restore index into mnemonic table
        ldx #3              // 3 letters in mnemonic
        jsr propxx          // print mnemonic
        ldx #6              // 6 possible address mode character combos
pradr1:
        cpx #3              // have we checked the third combo yet?
        bne pradr3          // if so, output the leading characters
        ldy length          // get the length of the operand
        beq pradr3          // if it's zero, there's no operand to print
pradr2:
        lda acmd            // otherwise, get the addressing mode
        cmp #$e8            // check for relative addressing
        php                 // save result of check
        lda (tmp2),y        // get the operand
        plp                 // restore result of check
        bcs relad           // handle a relative address
        jsr wrtwo           // output digits from address
        dey
        bne pradr2          // repeat for next byte of operand, if there is one
pradr3:
        asl acmd            // check whether addr mode uses the current char
        bcc pradr4          // if not, skip it
        lda char1-1,x       // look up the first char in the table
        jsr chrout          // print first char
        lda char2-1,x       // look up the second char in the table
        beq pradr4          // if there's no second character, skip it
        jsr chrout          // print second char
pradr4:
        dex                 // next potential address mode character
        bne pradr1          // loop if we haven't checked them all yet
        rts                 // back to caller
relad:
        jsr ub64d           // calculate absolute address from relative
        clc
        adc #1              // adjust address relative to next instruction
        bne relend          // don't increment high byte unless we overflowed
        inx                 // increment high byte
relend:
        jmp wraddr          // print address

ub64d:
        ldx tmp2+1          // get high byte of current address
        tay                 // is relative address positive or negative?
        bpl relc2           // if positive, leave high byte alone
        dex                 // if negative, decrement high byte
relc2:
        adc tmp2            // add relative address to low byte
        bcc relc3           // if there's no carry, we're done
        inx                 // if there's a carry, increment the high byte
relc3:
        rts

// -----------------------------------------------------------------------------
// get opcode mode and length

// note: the labels are different, but the code of this subroutine is almost
// identical to the insds2 subroutine of the apple mini-assembler on page 78 of
// the apple ii red book. i'm not sure exactly where this code originated
// (mos or apple) but it's clear that this part of supermon64 and the 
// mini-asssembler share a common heritage.  the comments showing the way the 
// opcodes are transformed into indexes for the mnemonic lookup table come
// from the mini-assembler source.

instxx:
        tay                 // stash opcode in accumulator in y for later
        lsr                 // is opcode even or odd?
        bcc ieven
        lsr  
        bcs err             // invalid opcodes xxxxxx11
        cmp #$22
        beq err             // invalid opcode 10001001
        and #$07            // mask bits to 10000xxx
        ora #$80
ieven:
        lsr                 // lsb determines whether to use left/right nybble
        tax                 // get format index using remaining high bytes
        lda mode,x
        bcs rtmode          // look at left or right nybble based on carry bit
        lsr                 // if carry = 0, use left nybble
        lsr  
        lsr  
        lsr  
rtmode:
        and #$0f            // if carry = 1, use right nybble
        bne getfmt
err:
        ldy #$80            // substitute 10000000 for invalid opcodes
        lda #0
getfmt:
        tax
        lda mode2,x         // lookup operand format using selected nybble
        sta acmd            // save for later use
        and #$03            // lower 2 bits indicate number of bytes in operand
        sta length
        tya                 // restore original opcode
        and #$8f            // mask bits to x000xxxx
        tax                 // save it
        tya                 // restore original opcode
        ldy #3
        cpx #$8a            // check if opcode = 1xxx1010
        beq gtfm4
gtfm2:
        lsr                 // transform opcode into index for mnemonic table
        bcc gtfm4
        lsr                 // opcodes transformed as follows:
gtfm3:
        lsr                 // 1xxx1010->00101xxx
        ora #$20            // xxxyyy01->00111xxx
        dey                 // xxxyyy10->00111xxx
        bne gtfm3           // xxxyy100->00110xxx
        iny                 // xxxxx000->000xxxxx
gtfm4:
        dey
        bne gtfm2
        rts

// -----------------------------------------------------------------------------
// extract and print packed mnemonics

propxx:

        tay                 // use index in accumulator to look up mnemonic
        lda mneml,y         //   and place a temporary copy in store
        sta store
        lda mnemr,y
        sta store+1
prmn1:
        lda #0              // clear accumulator
        ldy #$05            // shift 5 times
prmn2:
        asl store+1         // shift right byte
        rol store           // rotate bits from right byte into left byte
        rol                 // rotate bits from left byte into accumulator
        dey                 // next bit
        bne prmn2           // loop until all bits shifted
        adc #$3f            // calculate ascii code for letter by adding to '?'
        jsr chrout          // output letter
        dex                 // next letter
        bne prmn1           // loop until all 3 letters are output
        jmp space           // output space

// -----------------------------------------------------------------------------
// read parameters

rdpar:
        dec chrpnt          // back up one char
getpar:
        jsr rdval           // read the value
        bcs gterr           // carry set indicates error
        jsr gotchr          // check previous character
        bne ckterm          // if it's not null, check if it's a valid separator
        dec chrpnt          // back up one char
        lda digcnt          // get number of digits read
        bne getgot          // found some digits
        beq gtnil           // didn't find any digits
ckterm:
        cmp #$20            // space or comma are valid separators
        beq getgot          // anything else is an error
        cmp #','
        beq getgot
gterr:
        pla                 // encountered error
        pla                 // get rid of command vector pushed on stack
        jmp error           // handle error
gtnil:
        sec                 // set carry to indicate no parameter found
        .byte $24           // bit zp opcode consumes next byte (clc)
getgot:
        clc                 // clear carry to indicate paremeter returned
        lda digcnt          // return number of digits in a
        rts                 // return to address pushed from vector table

// -----------------------------------------------------------------------------
// read a value in the specified base

rdval:
        lda #0              // clear temp
        sta tmp0
        sta tmp0+1
        sta digcnt          // clear digit counter
        txa                 // save x and y
        pha
        tya
        pha
rdvmor:
        jsr getchr          // get next character from input buffer
        beq rdnilk          // null at end of buffer
        cmp #$20            // skip spaces
        beq rdvmor
        ldx #3              // check numeric base [$+&%]
gnmode:
        cmp hikey,x
        beq gotmod          // got a match, set up base
        dex
        bpl gnmode          // check next base
        inx                 // default to hex
        dec chrpnt          // back up one character
gotmod:
        ldy modtab,x        // get base value
        lda lentab,x        // get bits per digit
        sta numbit          // store bits per digit 
nudig:
        jsr getchr          // get next char in a
rdnilk:
        beq rdnil           // end of number if no more characters
        sec
        sbc #$30            // subtract ascii value of 0 to get numeric value
        bcc rdnil           // end of number if character was less than 0
        cmp #$0a
        bcc digmor          // not a hex digit if less than a
        sbc #$07            // 7 chars between ascii 9 and a, so subtract 7
        cmp #$10            // end of number if char is greater than f
        bcs rdnil
digmor:
        sta indig           // store the digit
        cpy indig           // compare base with the digit
        bcc rderr           // error if the digit >= the base
        beq rderr
        inc digcnt          // increment the number of digits
        cpy #10
        bne nodecm          // skip the next part if not using base 10
        ldx #1
declp1:
        lda tmp0,x          // stash the previous 16-bit value for later use
        sta stash,x
        dex
        bpl declp1
nodecm:
        ldx numbit          // number of bits to shift
times2:
        asl tmp0            // shift 16-bit value by specified number of bits
        rol tmp0+1
        bcs rderr           // error if we overflowed 16 bits
        dex
        bne times2          // shift remaining bits
        cpy #10
        bne nodec2          // skip the next part if not using base 10
        asl stash           // shift the previous 16-bit value one bit left
        rol stash+1
        bcs rderr           // error if we overflowed 16 bits
        lda stash           // add shifted previous value to current value
        adc tmp0
        sta tmp0
        lda stash+1
        adc tmp0+1
        sta tmp0+1
        bcs rderr           // error if we overflowed 16 bits
nodec2:
        clc 
        lda indig           // load current digit
        adc tmp0            // add current digit to low byte
        sta tmp0            // and store result back in low byte
        txa                 // a=0
        adc tmp0+1          // add carry to high byte
        sta tmp0+1          // and store result back in high byte
        bcc nudig           // get next digit if we didn't overflow
rderr:
        sec                 // set carry to indicate error
        .byte $24           // bit zp opcode consumes next byte (clc)
rdnil:
        clc                 // clear carry to indicate success
        sty numbit          // save base of number
        pla                 // restore x and y
        tay
        pla
        tax
        lda digcnt          // return number of digits in a
        rts

// -----------------------------------------------------------------------------
// print address

showad:
        lda tmp2
        ldx tmp2+1

wraddr:
        pha                 // save low byte
        txa                 // put high byte in a
        jsr wrtwo           // output high byte
        pla                 // restore low byte

wrbyte:
        jsr wrtwo           // output byte in a

space:
        lda #$20            // output space
        bne flip

chout:
        cmp #$0d            // output char with special handling of cr
        bne flip             //this line and above DIFF.
crlf:
        lda #$0d            // load cr in a
        bit $13             // check default channel
        bpl flip            // if high bit is clear output cr only
        jsr chrout          // otherwise output cr+lf
        lda #$0a            // output lf
flip:
        jmp chrout

fresh:
        jsr crlf            // output cr
        lda #$20            // load space in a
        jsr chrout
        jmp snclr

// -----------------------------------------------------------------------------
// output two hex digits for byte

wrtwo:

        stx savx            // save x
        jsr asctwo          // get hex chars for byte in x (lower) and a (upper)
        jsr chrout          // output upper nybble
        txa                 // transfer lower to a
        ldx savx            // restore x
        jmp chrout          // output lower nybble

// -----------------------------------------------------------------------------
// convert byte in a to hex digits

asctwo:

        pha                 // save byte
        jsr ascii           // do low nybble
        tax                 // save in x
        pla                 // restore byte
        lsr                 // shift upper nybble down
        lsr  
        lsr  
        lsr  

// convert low nybble in a to hex digit

ascii:
        and #$0f            // clear upper nibble
        cmp #$0a            // if less than a, skip next step
        bcc asc1
        adc #6              // skip ascii chars between 9 and a
asc1:
        adc #$30            // add ascii char 0 to value
        rts

// -----------------------------------------------------------------------------
// get prev char from input buffer

gotchr:
        dec chrpnt

// get next char from input buffer

getchr:
        stx savx
        ldx chrpnt          // get pointer to next char
        lda inbuff,x        // load next char in a
        beq nochar          // null, :, or ? signal end of buffer
        cmp #':'        
        beq nochar
        cmp #'?'
nochar:
        php
        inc chrpnt          // next char
        ldx savx
        plp                 // z flag will signal last character
        rts

// -----------------------------------------------------------------------------
// copy tmp0 to tmp2

copy12:
        lda tmp0            // low byte
        sta tmp2
        lda tmp0+1          // high byte
        sta tmp2+1
        rts

// -----------------------------------------------------------------------------
// subtract tmp2 from tmp0

sub12:
        sec
        lda tmp0            // subtract low byte
        sbc tmp2
        sta tmp0
        lda tmp0+1
        sbc tmp2+1          // subtract high byte
        sta tmp0+1
        rts

// -----------------------------------------------------------------------------
// subtract from tmp0

suba1:
        lda #1              // shortcut to decrement by 1
suba2:
        sta savx            // subtrahend in accumulator
        sec
        lda tmp0            // minuend in low byte
        sbc savx
        sta tmp0
        lda tmp0+1          // borrow from high byte
        sbc #0
        sta tmp0+1
        rts

// -----------------------------------------------------------------------------
// subtract 1 from store

sub13:
        sec
        lda store
        sbc #1              // decrement low byte
        sta store
        lda store+1
        sbc #0              // borrow from high byte
        sta store+1
        rts

// -----------------------------------------------------------------------------
// add to tmp2

adda2:
        lda #1              // shortcut to increment by 1
bumpad2:
        clc
        adc tmp2            // add value in accumulator to low byte
        sta tmp2
        bcc bumpex
        inc tmp2+1          // carry to high byte
bumpex:
        rts 

// -----------------------------------------------------------------------------
// subtract 1 from tmp2

sub21:
        sec
        lda tmp2            // decrement low byte
        sbc #1
        sta tmp2
        lda tmp2+1          // borrow from high byte
        sbc #0
        sta tmp2+1
        rts

// -----------------------------------------------------------------------------
// copy tmp0 to pc

copy1p:
        bcs cpy1px          // do nothing if parameter is empty
        lda tmp0            // copy low byte
        ldy tmp0+1          // copy high byte
        sta pcl
        sty pch
cpy1px:
        rts 

// -----------------------------------------------------------------------------
// get start/end addresses and calc difference

getdif:
        bcs gdifx           // exit with error if no parameter given
        jsr copy12          // save start address in tmp2
        jsr getpar          // get end address in tmp0
        bcs gdifx           // exit with error if no parameter given
        lda tmp0            // save end address in stash
        sta stash
        lda tmp0+1
        sta stash+1
        jsr sub12           // subtract start address from end address
        lda tmp0
        sta store           // save difference in store
        lda tmp0+1
        sta store+1
        bcc gdifx           // error if start address is after end address
        clc                 // clear carry to indicate success
        .byte $24           // bit zp opcode consumes next byte (sec)
gdifx:
        sec                 // set carry to indicate error
        rts

// -----------------------------------------------------------------------------
// convert base [$+&%]

convrt:
        jsr rdpar           // read a parameter
        jsr fresh           // next line and clear
        lda #'$'            // output $ sigil for hex
        jsr chrout
        lda tmp0            // load the 16-bit value entered
        ldx tmp0+1
        jsr wraddr          // print it in 4 hex digits
        jsr fresh
        lda #'+'            // output + sigil for decimal
        jsr chrout
        jsr cvtdec          // convert to bcd using hardware mode
        lda #0              // clear digit counter
        ldx #6              // max digits + 1
        ldy #3              // bits per digit - 1
        jsr nmprnt          // print result without leading zeros
        jsr fresh           // next line and clear
        lda #'&'            // print & sigil for octal
        jsr chrout
        lda #0              // clear digit counter
        ldx #8              // max digits + 1
        ldy #2              // bits per digit - 1
        jsr prinum          // output number
        jsr fresh           // next line and clear
        lda #'%'            // print % sigil for binary
        jsr chrout
        lda #0              // clear digit counter
        ldx #$18            // max digits + 1
        ldy #0              // bits per digit - 1
        jsr prinum          // output number
        jmp strt            // back to mainloop

// -----------------------------------------------------------------------------
// convert binary to bcd

cvtdec:
        jsr copy12          // copy value from tmp0 to tmp2
        lda #0
        ldx #2              // clear 3 bytes in work buffer
decml1:
        sta u0aa0,x
        dex
        bpl decml1
        ldy #16             // 16 bits in input
        php                 // save status register
        sei                 // make sure no interrupts occur with bcd enabled
        sed
decml2:
        asl tmp2            // rotate bytes out of input low byte
        rol tmp2+1          // .. into high byte and carry bit
        ldx #2              // process 3 bytes
decdbl:
        lda u0aa0,x         // load current value of byte
        adc u0aa0,x         // add it to itself plus the carry bit
        sta u0aa0,x         // store it back in the same location
        dex                 // decrement byte counter
        bpl decdbl          // loop until all bytes processed
        dey                 // decrement bit counter
        bne decml2          // loop until all bits processed
        plp                 // restore processor status
        rts

// load the input value and fall through to print it

prinum:
        pha                 // save accumulator
        lda tmp0            // copy input low byte to work buffer
        sta u0aa0+2
        lda tmp0+1          // copy input high byte to work buffer
        sta u0aa0+1
        lda #0              // clear overflow byte in work buffer
        sta u0aa0
        pla                 // restore accumulator

// print number in specified base without leading zeros

nmprnt:
        sta digcnt          // number of digits in accumulator
        sty numbit          // bits per digit passed in y register
digout:
        ldy numbit          // get bits to process
        lda #0              // clear accumulator
rolbit:
        asl u0aa0+2         // shift bits out of low byte
        rol u0aa0+1         // ... into high byte
        rol u0aa0           // ... into overflow byte
        rol                 // ... into accumulator
        dey                 // decrement bit counter
        bpl rolbit          // loop until all bits processed
        tay                 // check whether accumulator is 0
        bne nzero           // if not, print it
        cpx #1              // have we output the max number of digits?
        beq nzero           // if not, print it
        ldy digcnt          // how many digits have we output?
        beq zersup          // skip output if digit is 0
nzero:
        inc digcnt          // increment digit counter
        ora #$30            // add numeric value to ascii '0' to get ascii char
        jsr chrout          // output character
zersup:
        dex                 // decrement number of leading zeros
        bne digout          // next digit
        rts

// -----------------------------------------------------------------------------
// disk status/command [@]

dstat:
        bne chgdev          // if device address was given, use it
        ldx #8              // otherwise, default to 8
        .byte $2c           // absolute bit opcode consumes next word (ldx tmp0)
chgdev:
        ldx tmp0            // load device address from parameter
        cpx #4              // make sure device address is in range 4-31
        bcc ioerr
        cpx #32
        bcs ioerr
        stx tmp0
        lda #0              // clear status
        sta satus
        sta fnlen           // empty filename
        jsr getchr          // get next character
        beq instat1         // null, display status
        dec chrpnt          // back up 1 char
        cmp #'$'            // $, display directory
        beq direct
        lda tmp0            // command specified device to listen
        jsr listen
        lda #$6f            // secondary address 15 (only low nybble used)
        jsr second

// send command to device

dcomd:
        ldx chrpnt          // get next character from buffer
        inc chrpnt
        lda inbuff,x
        beq instat          // break out of loop if it's null
        jsr ciout           // otherwise output it to the serial bus
        bcc dcomd           // unconditional loop: ciout clears carry before rts

// get device status

instat:
        jsr unlsn           // command device to unlisten
instat1:
        jsr crlf            // new line
        lda tmp0            // load device address
        jsr talk            // command device to talk
        lda #$6f            // secondary address 15 (only low nybble used)
        jsr tksa
rdstat:
        jsr acptr           // read byte from serial bus
        jsr chrout          // print it
        cmp #$0d            // if the byte is cr, exit loop
        beq dexit
        lda satus           // check status
        and #$bf            // ignore eoi bit
        beq rdstat          // if no errors, read next byte
dexit:
        jsr untlk           // command device to stop talking
        jmp strt            // back to mainloop
ioerr:
        jmp error           // handle error

// get directory

direct:
        lda tmp0            // load device address
        jsr listen          // command device to listen
        lda #$f0            // secondary address 0 (only low nybble used)
        jsr second
        ldx chrpnt          // get index of next character
dir2:
        lda inbuff,x        // get next character from buffer
        beq dir3            // break if it's null
        jsr ciout           // send character to device
        inx                 // increment characer index
        bne dir2            // loop if it hasn't wrapped to zero
dir3:
        jsr unlsn           // command device to unlisten
        jsr crlf            // new line
        lda tmp0            // load device address
        pha                 // save on stack
        jsr talk            // command device to talk
        lda #$60            // secondary address 0 (only low nybble used)
        jsr tksa
        ldy #3              // read 3 16-bit values from device
dirlin:
        sty store           //   ignore the first 2// 3rd is file size
dlink:
        jsr acptr           // read low byte from device
        sta tmp0            // store it
        lda satus           // check status
        bne drexit          // exit if error or eof occurred
        jsr acptr           // read high byte from device
        sta tmp0+1          // store it
        lda satus           // check status
        bne drexit          // exit if error or eof cocurred
        dec store           // decrement byte count
        bne dlink           // loop if bytes remain
        jsr cvtdec          // convert last 16-bit value to decimal
        lda #0              // clear digit count
        ldx #6              // max 6 digits
        ldy #3              // 3 bits per digit
        jsr nmprnt          // output number
        lda #' '            // output space
        jsr chrout
dname:
        jsr acptr           // get a filename character from the device
        beq dmore           // if it's null, break out of loop
        ldx satus           // check for errors or eof
        bne drexit          // if found exit early
        jsr chrout          // output character
        clc
        bcc dname           // unconditional branch to read next char
dmore:
        jsr crlf
        jsr stop            // check for stop key
        beq drexit          // exit early if pressed
        jsr getin           // pause if a key was pressed
        beq nopaws
paws:
        jsr getin           // wait until another key is pressed
        beq paws            
nopaws:
        ldy #2
        bne dirlin          // unconditional branch to read next file
drexit:
        jsr untlk           // command device to untalk
        pla                 // restore accumulator
        jsr listen          // command device to listen
        lda #$e0            // secondary address 0 (only low nybble is used)
        jsr second
        jsr unlsn           // command device to unlisten
        jmp strt            // back to mainloop

// -----------------------------------------------------------------------------
// print and clear routines

cline:
        jsr crlf            // send cr+lf
        jmp snclr           // clear line
sndclr:
        jsr sndmsg
snclr:
        ldy #$28            // loop 40 times
snclp:
        lda #$20            // output space character
        jsr chrout
        lda #$14            // output delete character
        jsr chrout
        dey
        bne snclp
        rts

// -----------------------------------------------------------------------------
// display message from table

sndmsg:
        lda msgbas,y        // y contains offset in msg table
        php
        and #$7f            // strip high bit before output
        jsr chout
        iny
        plp
        bpl sndmsg          // loop until high bit is set
        rts

// -----------------------------------------------------------------------------
// message table// last character has high bit set

msgbas:
msg2:
        .byte $0d           // header for registers
        .text "   pc  sr ac xr yr sp   v1.2"
        .byte $0d+$80
msg3:
        .byte $1d,$3f+$80   // syntax error: move right, display "?"
msg4:
        .text "..sys"       // sys call to enter monitor
        .byte $20+$80
msg5:
        .byte $3a,$12+$80   // ":" then rvs on for memory ascii dump
msg6:
        .text " erro"       // i/o error: display " error"
        .byte 'r'+$80
msg7:
        .byte $41,$20+$80   // assemble next instruction: "a " + addr
msg8:
        .text "  "          // pad non-existent byte: skip 3 spaces
        .byte $20+$80

// -----------------------------------------------------------------------------
// addressing mode table - nybbles provide index into mode2 table
// for opcodes xxxxxxy0, use xxxxxx as index into table
// for opcodes wwwxxy01  use $40 + xx as index into table
// use right nybble if y=0// use left nybble if y=1

mode:
        .byte $40,$02,$45,$03// even opcodes
        .byte $d0,$08,$40,$09
        .byte $30,$22,$45,$33
        .byte $d0,$08,$40,$09
        .byte $40,$02,$45,$33
        .byte $d0,$08,$40,$09
        .byte $40,$02,$45,$b3
        .byte $d0,$08,$40,$09
        .byte $00,$22,$44,$33
        .byte $d0,$8c,$44,$00
        .byte $11,$22,$44,$33
        .byte $d0,$8c,$44,$9a
        .byte $10,$22,$44,$33
        .byte $d0,$08,$40,$09
        .byte $10,$22,$44,$33
        .byte $d0,$08,$40,$09
        .byte $62,$13,$78,$a9// opcodes ending in 01

// addressing mode format definitions indexed by nybbles from mode table

// left 6 bits define which characters appear in the assembly operand
// left 3 bits are before the address// next 3 bits are after

// right-most 2 bits define length of binary operand

// index               654 321
// 1st character       $(# ,),  
// 2nd character        $$ x y    length  format      idx mode

mode2:
        .byte $00           // 000 000    00                  0   error
        .byte $21           // 001 000    01      #$00        1   immediate
        .byte $81           // 100 000    01      $00         2   zero-page
        .byte $82           // 100 000    10      $0000       3   absolute
        .byte $00           // 000 000    00                  4   implied
        .byte $00           // 000 000    00                  5   accumulator
        .byte $59           // 010 110    01      ($00,x)     6   indirect,x
        .byte $4d           // 010 011    01      ($00),y     7   indirect,y
        .byte $91           // 100 100    01      $00,x       8   zero-page,x
        .byte $92           // 100 100    10      $0000,x     9   absolute,x
        .byte $86           // 100 001    10      $0000,y     a   absolute,y
        .byte $4a           // 010 010    10      ($0000)     b   indirect
        .byte $85           // 100 001    01      $00,y       c   zero-page,y
        .byte $9d           // 100 111    01      $0000*      d   relative

// * relative is special-cased so format bits don't match


// character lookup tables for the format definitions in mode2

char1:
        .byte $2c,$29,$2c   // ","  ")"  ","
        .byte $23,$28,$24   // "#"  "("  "$"

char2:
        .byte $59,$00,$58   // "y"   0   "x"
        .byte $24,$24,$00   // "$"  "$"   0

// -----------------------------------------------------------------------------
// 3-letter mnemonics packed into two bytes (5 bits per letter)

        // left 8 bits
        // xxxxx000 opcodes
mneml:
        .byte $1c,$8a,$1c,$23// brk php bpl clc
        .byte $5d,$8b,$1b,$a1// jsr plp bmi sec
        .byte $9d,$8a,$1d,$23// rti pha bvc cli
        .byte $9d,$8b,$1d,$a1// rts pla bvs sei
        .byte $00,$29,$19,$ae// ??? dey bcc tya
        .byte $69,$a8,$19,$23// ldy tay bcs clv
        .byte $24,$53,$1b,$23// cpy iny bne cld
        .byte $24,$53,$19,$a1// cpx inx beq sed
        // xxxyy100 opcodes
        .byte $00,$1a,$5b,$5b// ??? bit jmp jmp
        .byte $a5,$69,$24,$24// sty ldy cpy cpx
        // 1xxx1010 opcodes
        .byte $ae,$ae,$a8,$ad// txa txs tax tsx
        .byte $29,$00,$7c,$00// dex ??? nop ???
        // xxxyyy10 opcodes
        .byte $15,$9c,$6d,$9c// asl rol lsr ror
        .byte $a5,$69,$29,$53// stx ldx dec inc
        // xxxyyy01 opcodes
        .byte $84,$13,$34,$11// ora and eor adc
        .byte $a5,$69,$23,$a0// sta lda cmp sbc

        // right 7 bits, left justified
        // xxxxx000 opcodes
mnemr:
        .byte $d8,$62,$5a,$48// brk php bpl clc
        .byte $26,$62,$94,$88// jsr plp bmi sec
        .byte $54,$44,$c8,$54// rti pha bvc cli
        .byte $68,$44,$e8,$94// rts pla bvs sei
        .byte $00,$b4,$08,$84// ??? dey bcc tya
        .byte $74,$b4,$28,$6e// ldy tay bcs clv
        .byte $74,$f4,$cc,$4a// cpy iny bne cld
        .byte $72,$f2,$a4,$8a// cpx inx beq sed
        // xxxyy100 opcodes
        .byte $00,$aa,$a2,$a2// ??? bit jmp jmp
        .byte $74,$74,$74,$72// sty ldy cpy cpx
        // 1xxx1010 opcodes
        .byte $44,$68,$b2,$32// txa txs tax tsx
        .byte $b2,$00,$22,$00// dex ??? nop ???
        // xxxyyy10 opcodes
        .byte $1a,$1a,$26,$26// asl rol lsr ror
        .byte $72,$72,$88,$c8// stx ldx dec inc
        // xxxyyy01 opcodes
        .byte $c4,$ca,$26,$48// ora and eor adc
        .byte $44,$44,$a2,$c8// sta lda cmp sbc
        .byte $0d,$20,$20,$20

// -----------------------------------------------------------------------------
// single-character commands

keyw:
        .text "acdfghjmrtx@.>;"
hikey:
        .text "$+&%lsv"
keytop:

// vectors corresponding to commands above

kaddr: 
        .word assem-1,compar-1,disass-1,fill-1
        .word goto-1,hunt-1,jsub-1,dsplym-1
        .word dsplyr-1,trans-1,exit-1,dstat-1
        .word assem-1,altm-1,altr-1

// -----------------------------------------------------------------------------

modtab:
        .byte $10,$0a,$08,02// modulo number systems
lentab:
        .byte $04,$03,$03,$01// bits per digit

linkad:
        .word break         // address of brk handler
supad:
        .word super         // address of entry point