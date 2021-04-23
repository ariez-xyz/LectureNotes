#!/usr/local/anaconda3/bin/ipython
# coding: utf-8

from experiments import *
import sys

c = read_binary(sys.argv[1])

print("running xor against shifted cipher...")

iocs = []
n = 1

while True:
    ioc = index_of_coincidence(xor(c,c,shift=n))
    #print("\t", n, "\t", ioc)
    iocs.append(ioc)
    if max(iocs) > min(iocs) * 1.25:
        print(f"found large IoC {ioc} for shift value {n}. assuming key length L={n}.")
        break
    n += 1
    
print(f"splitting cipher into {n} sets...")

sets = [c[i::n] for i in range(n)]

print("deriving key assuming most frequent char is ' '...")

key = b""

for s in sets:
    counts = count_freqs(s)
    space_char = max(counts, key=counts.get) # space_char holds what ' ' is mapped to
    key += bytes([space_char ^ ord(" ")])

print(f"key candidate: {key}")
print("decrypted message:")
print(xor(c, key))

