#! /usr/bin/env python3

# P register: SV..DIZC
# ID

import numpy as np
import binascii
from matplotlib import pyplot as plt

filename = "OPCDATA.BIN"

# Read data

with open(filename, "rb") as f:
    data = f.read()

# Verify checksum

checksum = binascii.crc32(data)
assert checksum == (1 << 32) - 1

# Discard checksum

data = data[:-4]
# Convert to numpy structured array type

rectype = [
    ("P"    , np.uint8), # input
    ("OP1"  , np.uint8), # input
    ("OP2"  , np.uint8), # input
    ("ORA_A", np.uint8), # understood: bitwise OR
    ("ORA_P", np.uint8), # understood: set N and Z according to result.
    ("AND_A", np.uint8), # understood: bitwise AND
    ("AND_P", np.uint8), # understood: set N and Z according to result.
    ("EOR_A", np.uint8), # understood
    ("EOR_P", np.uint8), # understood: set N and Z according to result.
    ("ADC_A", np.uint8),
    ("ADC_P", np.uint8),
    ("STA_A", np.uint8), # understood
    ("STA_P", np.uint8), # understood: status word unaffected
    ("LDA_A", np.uint8), # understood
    ("LDA_P", np.uint8), # understood: set N and Z according to result.
    ("CMP_A", np.uint8), # understood
    ("CMP_P", np.uint8), # understood
    ("SBC_A", np.uint8),
    ("SBC_P", np.uint8),
    ("BIT_A", np.uint8), # understood
    ("BIT_P", np.uint8)
]

data = np.frombuffer(data, dtype = rectype)

# simple

if False:
    assert all(data["ORA_A"] == data["OP1"] | data["OP2"]) # understood: bitwise OR
    assert all(data["AND_A"] == data["OP1"] & data["OP2"]) # understood: bitwise AND
    assert all(data["EOR_A"] == data["OP1"] ^ data["OP2"]) # understood: bitwise XOR
    assert all(data["STA_A"] == data["OP1"]              ) # understood: no change
    assert all(data["LDA_A"] == data["OP2"]              ) # understood: no change (first version did LDA OP2).
    assert all(data["CMP_A"] == data["OP1"]              ) # understood: no change
    assert all(data["BIT_A"] == data["OP1"]              ) # understood: no change

# NZ updated to result

if False:
    assert all(data["ORA_P"] == data["P"] & (~0x82) | 0x30 | (data["ORA_A"] == 0) << 1 | data["ORA_A"] & 0x80)
    assert all(data["AND_P"] == data["P"] & (~0x82) | 0x30 | (data["AND_A"] == 0) << 1 | data["AND_A"] & 0x80)
    assert all(data["EOR_P"] == data["P"] & (~0x82) | 0x30 | (data["EOR_A"] == 0) << 1 | data["EOR_A"] & 0x80)
    assert all(data["STA_P"] == data["P"]           | 0x30                                                   )
    assert all(data["LDA_P"] == data["P"] & (~0x82) | 0x30 | (data["LDA_A"] == 0) << 1 | data["LDA_A"] & 0x80)
    assert all(data["BIT_P"] == data["P"] & (~0xc2) | 0x30 | ((data["OP1"] & data["OP2"]) == 0) << 1 | data["OP2"] & 0xc0)

    print("ok")

## Check ADC and SBC in binary mode

def u2s(x):
    return x - 2 * (x & 0x80).astype(int)

def in_signed_range(x):
    return (x >= -128) & (x <= 127)

data_bin = data[data["P"] & 0x08 == 0]
print(len(data_bin))

assert all(data_bin["ADC_A"] == data_bin["OP1"] + data_bin["OP2"] + (data_bin["P"] & 0x01))
assert all(data_bin["ADC_P"] == data_bin["P"] & (~0xc3) | 0x30
           | (data_bin["ADC_A"] & 0x80)
           | in_signed_range(u2s(data_bin["OP1"]) + u2s(data_bin["OP1"]) + data_bin["P"] & 1) << 6
           | (data_bin["ADC_A"] == 0) << 1
           | (data_bin["OP1"] + data_bin["OP2"] + (data_bin["P"] & 1))
    )



#           ((data["OP1"] & data["OP2"]) == 0) << 1 | data["OP2"] & 0xc0)


#ORA_P = data["P"] | ((data["ORA_A"] == 0) * 2).astype(np.uint8) | (data["ORA_A"] & 0x80).astype(np.uint8)

#print(  ((data["ORA_A"] == 0) * 2).astype(np.uint8))

if False:
    for z in data:
        msm  = z["BIT_P"]
        pred = z["P"] & (~0xc2) | 0x30 | (z["OP1"] & z["OP2"] == 0) << 1 | z["OP2"] & 0xc0
        if pred != msm:
            print("P {:02x} OP1 {:02x} OP2 {:02x} BIT_P {:02x} pred {:02x}".format(
                z["P"], z["OP1"], z["OP2"], z["BIT_P"], pred))

#assert all(ORA_P == data["ORA_P"])

#plt.scatter(data["ORA_P"], ORA_P, edgecolor = '')
#plt.show()

#assert all(data["ADC_A"] == data["OP1"] + data["OP2"] + (data["P"] & 1))

#plt.plot(data["P"], '.')
#plt.show()
