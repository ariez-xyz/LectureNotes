#!/usr/local/anaconda3/bin/ipython
# coding: utf-8

from experiments import *
import sys

m = read_binary(sys.argv[1])
k = sys.argv[2].encode()
c = xor(m, k)

with open(sys.argv[3], "wb") as f:
    f.write(c)


