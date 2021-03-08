import sys
import matplotlib.pyplot as plt
import matplotlib as mpl
from dateutil.parser import parse

mpl.rcParams['figure.dpi'] = 200
plt.ylabel("seq")
plt.xlabel("ms")
            
timestamps = []
seqs = []

with open(sys.argv[1]) as f:
    prev_seq = 10e20 # safe high number
    n_plots = 0
    
    for line in f.readlines()[1:]: # ignore header
        split = line.strip().split("\t")
        
        if len(split) == 2:
            # parse seq and timestamp
            seq = int(split[1].split(":")[0])
            timestamp = parse(split[0]).timestamp()
            
            # when a transfer has been parsed, plot it
            if seq < prev_seq:
                # ignore bad data
                if len(timestamps) > 500:
                    n_plots += 1
                    plt.plot(timestamps, seqs, label=f"Lauf {n_plots}")
                if n_plots == 10:
                    break
                timestamps = []
                seqs = []
                initial_timestamp = timestamp
                
            prev_seq = seq
            timestamps.append((timestamp - initial_timestamp) * 1000)
            seqs.append(seq)
            
plt.legend()
plt.show()

