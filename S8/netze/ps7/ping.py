#!/bin/python3

import sys
import subprocess

print("Name,Ort,Domain,IP,Min,Avg,Max,Mdev")

with open(sys.argv[1]) as f:
    f.readline()
    for line in f.readlines():
        name, location, domain = line.strip().split(",")
        ips = subprocess.check_output(['dig', '+short', domain]).decode("ascii") 
        ip = ips.splitlines()[-1]
        ping = subprocess.check_output(['ping', '-qc', '10', domain]).decode("ascii")
        summary = ping.splitlines()[-1]
        data = summary.split(" ")[3].replace("/", ",")
        print(name, location, domain, ip, data, sep=",")
