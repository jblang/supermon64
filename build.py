#!/usr/bin/env python

# Tool to build relocatable Supermon+64 binary

# Jim Butterfield described the post-processing of Supermon 64 binaries
# in a posting to comp.sys.cbm on Dec 20, 2003:

# I should note that, since Supermon+64 is relocatable code, the source
# does not assemble into the final binary file.  It may seem crude, but
# I follow this procedure:  (1) The source is carefully structured so
# that there are no "dispersed addresses" such as might be created with
# something like LDA #>VECTOR .. LDY #<VECTOR - every relocatable
# address is two adjacent bytes;  (2) I assemble the source TWICE, to
# two different page addresses; the only difference in the binaries will
# be the high-order bytes of the relocatable addresses; (3) a small
# post-processing program blends the two binaries into a relocatable
# package, adding a Basic driver to complete the bundle.

# Source: https://groups.google.com/forum/#!searchin/comp.sys.cbm/supermon$2064%7Csort:relevance/comp.sys.cbm/5owItyf5qjk/50_UQlwVnPcJ

# This python script performs the procedure given above. It expects 4
# parameters: the relocation stub, assembled from relocate.asm, two raw
# supermon64 binaries assembled to two different origins, and the name
# of the final output.


import sys
import os
import struct

if len(sys.argv) < 5:
    print("usage: %s relocate.prg location1.prg location2.prg output.prg" % sys.argv[0])
    sys.exit()

stub = open(sys.argv[1], 'rb').read()
mon1 = open(sys.argv[2], 'rb').read()
mon2 = open(sys.argv[3], 'rb').read()

if len(mon1) != len(mon2):
    print("input files are not the same length")
    sys.exit()

# get end of first program from header
end1 = struct.unpack('H', mon1[0:2])[0] + len(mon1) - 2

# strip origin from both programs
mon1 = mon1[2:]
mon2 = mon2[2:]

with open(sys.argv[4], 'wb') as out:
    # prepend relocator stub
    out.write(stub)
    # separate from supermon code with $36 twice
    out.write(b'\x36\x36')
    i = 0
    while i < len(mon1):
        if len(mon1) > i+1 and mon1[i+1] != mon2[i+1]:
            addr = struct.unpack('H', mon1[i:i+2])[0]
            # calculate offset to address
            offset = struct.pack('h', addr - end1)
            out.write(offset)
            # mark address to be adjusted with $36
            out.write(b'\x36')
            i += 2
        else:
            out.write(mon1[i:i+1])
            i += 1
