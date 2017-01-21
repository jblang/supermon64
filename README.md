# Supermon+64 V1.2
By Jim Butterfield

## Background
Supermon64 is a machine-language monitor for the Commodore 64 written by [Jim Butterfield](https://en.wikipedia.org/wiki/Jim_Butterfield). It originally appeared as a type-in program in the [January 1983 issue of Compute Magazine](https://archive.org/details/1983-01-compute-magazine). The program was widely distributed by Commodore User's Groups and included on the [demo diskettes](http://www.zimmers.net/anonftp/pub/cbm/demodisks/c64/starter-kit.d64.gz) and tapes that Commodore provided with their hardware.  An improved version, 1.2, appeared sometime later.

After working my way through Jim Butterfield's excellent [Machine Language for the Commodore 64](https://archive.org/details/Machine_Language_for_the_Commodore_Revised_and_Expanded_Edition) using his equally excellent monitor, I set out to try to find the original sources for it. For a piece of public domain software, it wasn't as easy as you'd think.  

I found [this thread](http://comp.sys.cbm.narkive.com/KUAL6oqM/attn-jim-butterfiled-i-m-looking-for-supermon-64-source-code) where someone asked for the sources and Jim Butterfield himself responded. One person pointed to this [modified version](http://www.ffd2.com/fridge/programs/supermon.s) but I wanted the original. Someone else mentioned a file called SPRMON64.SDA at a now-defunct FTP site.  I searched for the filename and found it in on a [gopher proxy](https://gopherproxy.meulie.net/sdf.org/1/users/rogertwo/prgs/cbm/c64/programming/).  

I downloaded it and it appears to be the original source and binaries for the updated 1.2 version.  Since it doesn't seem to have an official home and it's continued availability on the internet seemed precarious, I decided to give it one on GitHub.

## Contents

- [sprmon64_email.txt](sprmon64_email.txt) is the file I downloaded. It is not actually the SDA itself, but an email containing the SDA in a uuencoded attachment.
- [sprmon64.sda](sprmon64.sda) is the actual SDA that I uudecoded from the email I downloaded.
- [sprmon64.d64](sprmon64.d64) - the SDA converted to a d64 image for use in VICE or with sd2iec.

The original files extracted from the SDA:
- [supermon64.prg](supermon64.prg) is the C64 binary for Supermon 1.2.
- [supermon64 inst.prg](supermon64 inst.prg) is a C64 BASIC program that displays instructions.
- [supermon64 docs.seq](supermon64 docs.seq) is a PETSCII file containing instructions.
- [supermon64.src.seq](supermon64.src.seq) is the PETSCII source code. According to Jim's [email](http://comp.sys.cbm.narkive.com/KUAL6oqM/attn-jim-butterfiled-i-m-looking-for-supermon-64-source-code), these sources are for the [PAL Assembler](https://ist.uwaterloo.ca/~schepers/download.html) by Brad Templeton.

Converted versions of the original files:
- [supermon64 docs.txt](supermon64 docs.txt) is the PETSCII documentation converted to ASCII.
- [supermon64.src.txt](supermon64.src.txt) is the PETSCII source converted to ASCII.
- [supermon64.asm](supermon64.asm) is the source with fixed indentation and minor tweaks to get it to build successfully in [64tass](https://sourceforge.net/projects/tass64/) and [TMPx](http://style64.org/release/tmpx-v1.1.0-style).  I tested the resulting program and it works, but I can't guarantee it's 100% correct.  I have worked my way through the code and commented it to the best of my understanding. The comments are still a work in progress.

## License

Based on multiple sources, this software appears to be in the public domain.  I claim no ownership.

The comments in supermon64.asm are my own. I hereby place them in the public domain. However, I would greatly appreciate attribution if you make use of them.
