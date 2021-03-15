# coding: utf-8

def read_binary(path):
    with open(path, "rb") as f:
        return f.read()
        
def index_of_coincidence_sample(s, samples=100000):
    hits = 0
    tries = 0
    for i in range(0,samples):
        i1 = random.randint(0, len(s) - 1)
        i2 = random.randint(0, len(s) - 1)
        if i1 == i2:
            continue
        tries += 1
        if s[i1] == s[i2]:
            hits += 1
    return 1.0*hits/tries
    
def count_freqs(s):
    counts = {}
    for letter in s:
        if letter not in counts.keys():
            counts[letter] = 0
        counts[letter] += 1
    return counts

def index_of_coincidence(s):
    counts = count_freqs(s)
    total = 0
    for count in counts.values():
        total += count * (count - 1)
    return total / (len(s) * (len(s) - 1))

def xor(s, k, shift=0):
    r = b""
    for i in range(len(s)):
        r += bytes([s[(i + shift) % len(s)] ^ k[i % len(k)]])
    return r
