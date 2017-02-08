# Supermon+64 V1.2
By Jim Butterfield et. al.

Supermon64 is a machine-language monitor for the Commodore 64.  In modern parlance, it would be
called a debugger, providing functions including inspecting and altering registers and memory locations;
searching, comparing, and transferring blocks of memory; and assembling and disassembling machine code.

Here is a 10-minute video I made demonstrating many of its features: https://www.youtube.com/watch?v=MEjnMt_3wkU

## Contents

Original artifacts:
- [sprmon64.txt](sprmon64.txt) is a posting to comp.binaries.cbm
  containing a uuencoded self-dissolving archive with the Supermon+64 V1.2
  sources, instructions and binaries.
- [sprmon64.d64](sprmon64.d64) is a D64 image created from the 
  archive for use with VICE, disk drive emulators, or to create a real floppy disk.
- [supermon64.prg](supermon64.prg) is the original Supermon+64 binary taken from
  the archive. The sources below will build an identical binary.

Modernized sources:
- [supermon64.asm](supermon64.asm) is the Supermon+64 V1.2 source code, converted to 64tass format
 and heavily commented by me. 
- [relocate.asm](relocate.asm) is the disassembled machine-code stub that loads Supermon+64 into
  the top of BASIC memory.
- [build.py](build.py) is a python script I wrote that transforms the assembled relocator
  stub and the fixed-location Supermon binaries into a relocatable binary.
- [Makefile](Makefile) is a GNU makefile that will build the final supermon64.prg binary
  using the above sources.

## Background

Supermon is closely associated with [Jim Butterfield](https://en.wikipedia.org/wiki/Jim_Butterfield)
but it had many contributors over the years.  The original version of Supermon for the Commodore PET
contained the following credits:

- Dissassembler by Wozniak/Baum
- Single step by Jim Russo
- Most other stuff (,HAFT) by Bill Seiler
- Tidied & Wrapped by Jim Butterfield

The earliest documented appearance of Supermon that I could find was in the 
[January 1980 issue of The Transactor](https://archive.org/details/transactor-magazines-v2-i08). 
From its origins on the PET, Supermon made its way to the VIC and the Commodore 64. It apparently
shares some DNA with the monitor and mini assembler on the Apple II as well as Micromon 
and MADS monitors on various Commodore computers.

The first version for the Commodore 64 appeared as a type-in program in the 
[January 1983 issue of Compute Magazine](https://archive.org/details/1983-01-compute-magazine). 
An improved version followed in 1985, updated to include the features in the built-in monitors
for the Commodore Plus/4 and 128.  This is the version that is preserved here.

Supermon 64 was widely distributed by Commodore User's Groups and included on the 
[demo diskettes](http://www.zimmers.net/anonftp/pub/cbm/demodisks/c64/starter-kit.d64.gz)
and tapes that Commodore provided with their hardware.  It was also included as a type-in
program in the back of many books on machine language programming, including Rae West's
[Programming the Commodore 64: The Definitive Guide](https://archive.org/download/Compute_s_Programming_the_Commodore_64_The_Definitive_Guide/Compute_s_Programming_the_Commodore_64_The_Definitive_Guide.pdf)
and Jim Butterfield's own 
[Machine Language for the Commodore 64](https://archive.org/details/Machine_Language_for_the_Commodore_Revised_and_Expanded_Edition).

More than 30 years later, I decided to learn 6502 assembly. After working my way through 
Jim Butterfield's excellent book using Supermon64, I wanted to find some real assembly code to 
study, and the software I had just been using seemed like a natural place to start.  I started 
looking for the sources online but for a piece of public domain software, it wasn't as easy as
to find as you'd think.  

I found [this thread](http://comp.sys.cbm.narkive.com/KUAL6oqM/attn-jim-butterfiled-i-m-looking-for-supermon-64-source-code)
on comp.sys.cbm where someone asked for the sources and Jim Butterfield himself responded, but
at the time he didn't have easy access to the sources. One person pointed to this
[modified version](http://www.ffd2.com/fridge/programs/supermon.s)
but I wanted the original.  Someone else mentioned a file called SPRMON64.SDA at a now-defunct FTP
site.  I googled for the filename and found what was apparently the last remaining copy on the internet
on a [gopher proxy](https://gopherproxy.meulie.net/sdf.org/1/users/rogertwo/prgs/cbm/c64/programming/).

I downloaded it and found that it contains what appear to be the original source and binaries
for the updated 1.2 version.  Since it doesn't seem to have an official home and it's 
continued availability on the internet seemed precarious, I decided to give it one on GitHub.

I modernized the source code so it can be built using the [64tass](https://sourceforge.net/projects/tass64/)
cross-assembler. The original source (included on the D64 image) was almost completely uncommented and used a
few constructs that were unsupported by 64tass.  I converted these to the equivalent 64tass constructs, 
indented the code, and then worked my way through the code line-by-line until I understood it all, commenting
it as I went along.

As Jim noted in his usenet posting, the provided sources don't produce the final Supermon 64 binary:

>I should note that, since Supermon+64 is relocatable code, the source
does not assemble into the final binary file.  It may seem crude, but
I follow this procedure:  (1) The source is carefully structured so
that there are no "dispersed addresses" such as might be created with
something like LDA #>VECTOR .. LDY #<VECTOR - every relocatable
address is two adjacent bytes;  (2) I assemble the source TWICE, to
two different page addresses; the only difference in the binaries will
be the high-order bytes of the relocatable addresses; (3) a small
post-processing program blends the two binaries into a relocatable
package, adding a Basic driver to complete the bundle.

Neither the sources for the relocator nor the post-processing program Jim refers to were included
in the archive, so I disassembled the original supermon64.prg binary and 
reconstructed the relocator stub from that. I also wrote a simple python script that builds a
relocatable binary according to Jim's instructions above.  I have confirmed that the
binary produced by assembling `supermon64.asm` and `relocator.asm` and then combining them using
`build.py` is identical to the original binary `supermon64.prg` provided in the archive.

I  converted the usage instructions from a PETSCII file in the archive to the Markdown
below.  Lastly, I researched and documented the history of the code that you are reading now.
I hope you enjoy this piece of computing history!

## Usage Instructions

SUPERMON+ is a new version of 'SUPERMON'. The reason for the new
version is to provide identical commands to those of the built-in
monitor of the Commodore 128.

The most visible changes from earlier versions of SUPERMON are:

  - decimal or binary input allowed;
  - disk status and commands (@);
  - looser (easier) syntax.

### Number Conversion 

```
$2000
      $2000
      +8192
      &20000
      %10000000000000
```

In the above example the user has asked for the numeric
equivalents to hexadecimal 2000.  The reply shows the value in hex
($), in decimal (+), in octal (&) and in binary (%).

The user could ask for a number to be converted from any of these
bases by giving the appropriate prefix.

IMPORTANT NOTE -- At any time in the following text, you may enter
any number in any base and conversion will be done for you.

Example:

```
m +4096
```

Will cause a memory display from decimal address 4096. In the
display, the hex address ($1000) will be shown. Similarly,

```
+2048 lda#%10000000
```

Will be converted to assemble: "a $0400 lda #$80"

If you don't give a prefix, the monitor will assume hexadecimal.


### Register Display

```
r

   pc  sr ac xr yr sp
; 0000 01 02 03 04 05
```

Displays the register values saved when SUPERMON+ was entered.
Values may be changed by typing over the display followed by a
return character.

pc - program counter
sr - status register
ac, xr, yr - a, x, and y registers
sp - stack pointer

### Memory Display

```
m 200 209

>0200 4d 20 32 30 30 20 32 30: m 200 20
>0208 39 00 00 04 00 04 00 04: 9.......
```

Display memory from 0200 hex to 0209 hex. Display is in lines of
8, so addresses $200 to $20f are shown. If only one address is
used then 12 lines (96 locations) will be shown. If no address is
given display will go from the last address. Equivalent ASCII
characters are shown in reverse at the right. Values are changed
by typing over the display followed by a return character.


### Exit to BASIC

```
x
```

Return to BASIC READY mode. When you wish to return to SUPERMON+,
command "SYS 8".  

### Simple Assembler

```
a 2000 lda #+18
```

changes  to:

```
a 2000 a9 12    lda #$12
a 2002  ..next instruction
```

In the above example the user started assembly at 2000 hex. The
first instruction was load a register with immediate 18
decimal. In following lines the user need not type the "a" and
address. The simple assembler prompts with the next address. To
exit the assembler type a return after the the address prompt.

Previous lines may be changed by typing over the right hand part.

### Disassembler

```
d 2000 2004

. 2000 a9 12    lda #$12
. 2002 9d 00 80 sta $8000,x
```

Disassembles instructions from 2000 to 2004 hex. If one address
is given, 20 bytes will be disassembled. If no address, 
start from the last used address.  

Code may be reassembled by moving the cursor back and typing over
the right hand part.


### Fill Memory

```
f 1000 1100 ff
```

fills the memory from 1000 hex to 1100 hex with the byte ff hex.

### Go (run)

```
g 1000
```

Go to address 1000 hex and begin running code. If no address is
given, the address from the <pc> register is used.

### Jump (subroutine)

```
j 1000
```

Call address 1000 hex and begin running code. Return to the
monitor.

### Hunt Memory 

```
h c000 d000 'read
```

Hunt thru memory from c000 hex to d000 hex for the ascii string
"read" and print the address where it is found. A maximum of
32 characters may be used.

```
h c000 d000 20 d2 ff
```

Hunt memory from c000 hex to d000 hex for the sequence of bytes
20 d2 ff and print the address. A maximum of 32 bytes may be used.

### File Handling

#### Load 

```
l
```
Load any program from cassette #1.

```
l "ram test"
```

Load from cassette #1 the program named "ram test".

```
l "ram test",08
```

Load from disk (device 8) the program named  "ram test". This
command leaves basic pointers unchanged.

#### Save

```
s "program name",01,0800,0c80
```

Save to cassette #1 memory from 0800 hex up to but not including
0c80 hex and name it "program name".

```
s "0:program name",08,1200,1f50
```
     
Save to disk drive #0 memory from 1200 hex up to but not including
1f50 hex and name it "program name".

### Transfer Memory 

```
t 1000 1100 5000
```

Transfer memory in the range 1000 hex to 1100 hex and start storing
it at address 5000 hex.  

### Compare Memory

```
c 1000 1100 5000
```

Compare memory in the range 1000 hex to 1100 hex with memory
starting at address 5000 hex.  

### Disk Operations

```
@
```

Get disk status message

```
@9
```

Get disk unit 9 status message

```
@,$0
```

Get drive 0 directory

```
@,s0:temp
```

Scratch file 'temp' from disk

### Output to Printer

Call SUPERMON+ from basic with:
```
open 4,4:cmd 4:sys 8
```
All commands will go the printer.  When complete, return to basic
with "x" and command:

```
print#4:close 4
```


### Summary

- `$ , + , & , %`  number conversion
- `g`  go (run)
- `j`  jump  (subroutine)
- `l`  load from tape or disk
- `m`  memory display
- `r`  register display
- `s`  save to tape or disk
- `x`  exit to basic
- `a`  simple assembler
- `d`  disassembler
- `f`  fill memory
- `h`  hunt memory
- `t`  transfer memory
- `c`  compare memory
- `@`  disk status/command

### Restarting Supermon

Supermon will load itself into the top of memory...wherever that happens to
be on your machine. Be sure to note the SYS command which links SUPERMON
to the Commodore. It may be used to reconnect the monitor if it is
accidentally disconnected by use of the run-stop/restore keys.

## License

To the best of my knowledge, this software is in the public domain.  I claim no ownership.

The comments in `supermon64.asm` and `relocate.asm` as well as the entirety of `build.py` are my own.
I hereby place them in the public domain. However, I would greatly appreciate attribution if you make use of them.
