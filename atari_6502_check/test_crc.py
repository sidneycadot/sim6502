#! /usr/bin/env python3

import binascii

with open("OPCDATA.BIN", "rb") as f:
    data = f.read()

print(type(data))

print("{:x}".format(binascii.crc32(data)))
