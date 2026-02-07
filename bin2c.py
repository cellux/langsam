#!/usr/bin/env python3

import sys

infile = sys.argv[1]
outfile = sys.argv[2]
symbol = sys.argv[3]

with open(infile, "rb") as f:
    data = f.read()

with open(outfile, "w") as f:
    f.write(f"int {symbol}_size = {len(data)};\n")
    f.write(f"char {symbol}_data[] = {{")
    i = 0
    while i < len(data):
        if i % 16 == 0:
            f.write("\n  ")
        f.write(f"0x{data[i]:02x}")
        i += 1
        if i < len(data):
            f.write(",")
        else:
            f.write("\n")
    f.write("};\n")
