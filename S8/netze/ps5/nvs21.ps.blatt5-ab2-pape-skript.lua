#!/usr/bin/lua

s = "NVS2021"

-- UTF16BE bits of "NVS2021", from https://onlineutf8tools.com/convert-utf8-to-utf16
bin = "\z
0000000001001110\z
0000000001010110\z
0000000001010011\z
0000000000110010\z
0000000000110000\z
0000000000110010\z
0000000000110001\z
"

-- same for hex
hex = "\z
004E\z
0056\z
0053\z
0032\z
0030\z
0032\z
0031\z
"


print([[

#######################
# Manchester Encoding #
#######################

0 is lo-hi; 1 is hi-lo. Encode "NVS2021":
]])

for i = 1, #bin do
  c = bin:sub(i,i) -- get i-th char
  if c == "0" then io.write("01")
  else io.write("10") end
end



print([[



##############
# 4B5B/MLT-3 #
##############

4B5B encoded "NVS2021":
]])

translate_4b5b = {}
translate_4b5b["0"] = 11110
translate_4b5b["1"] = 01001
translate_4b5b["2"] = 10100
translate_4b5b["3"] = 10101
translate_4b5b["4"] = 01010
translate_4b5b["5"] = 01011
translate_4b5b["6"] = 01110
translate_4b5b["7"] = 01111
translate_4b5b["8"] = 10010
translate_4b5b["9"] = 10011
translate_4b5b["A"] = 10110
translate_4b5b["B"] = 10111
translate_4b5b["C"] = 11010
translate_4b5b["D"] = 11011
translate_4b5b["E"] = 11100
translate_4b5b["F"] = 11101


encoded_4b5b = ""
for i = 1, #hex do
  c = hex:sub(i,i)
  encoded_4b5b = encoded_4b5b .. translate_4b5b[c]
end

print(encoded_4b5b .. "\n")



print([[
Ethernet 100base-tx ist sehr nah an ANSI X3.263 angelehnt, welches MLT-3 als letztes
Encoding vor der Übertragung des Datenstreams verwendet. 
Selbiges gilt für Ethernet. Gründe für die Verwendung von MLT-3 sind gerinere 
elektromagnetische Interferenz und höhere Bandbreite verglichen 
mit z.B. dem Manchester Encoding. MLT-3 setzt aber voraus, dass es keine zu langen Serien
von 0 bits gibt, da es keine Transition bei einer 0 gibt.
Durch die vorherige Kodierung mit 4B5B ist dies gegeben.

Reencoded with MLT-3:
]])


mlt3_sequence = "-0+0"
mlt3_pos = 1

for i = 1, #encoded_4b5b do
  c = encoded_4b5b:sub(i,i)
  if c == "1" then 
    mlt3_pos = mlt3_pos + 1 
    if mlt3_pos > #mlt3_sequence then
      mlt3_pos = mlt3_pos - #mlt3_sequence
    end
  end
  io.write(mlt3_sequence:sub(mlt3_pos, mlt3_pos))
end
print("\n")


print([[

###########
# CSMA/CD #
###########

    C      C      C    
    |      |      |
----┴------┴------┴----

Mehrere Rechner sind in einem (non-duplex) Netzwerk miteinander verbunden. CSMA/CD ist eine
Methode, um Kollisionen zu vermeiden: Teilnehmer warten darauf, dass die Leitung frei ist,
bevor sie ein Signal senden. So werden Kollisionen vermieden.


    C      C      C    
    ║      |      |
════╩══════╧══════╧════

Ist die Leitung frei, kann ohne Probleme gesendet werden.


    C      C      C    
    ║      ║      |
xxxx╩xxxxxx╩xxxxxxxxxxx

Senden zwei Teilnehmer genau gleichzeitig, passiert dennoch eine Kollision.
Die Kollision kann von den Sendern erkannt werden, weil das Signal auf der Leitung anders
ist als jenes, was gerade gesendet wird. Dann wird die Jam Sequence gesendet, Teilnehmer
warten eine zufällige Zeit, und versuchen danach erneut zu senden.

Begriffe:

Carrier Sense bedeutet, dass mit dem Netzwerk verbundene Rechner feststellen können, ob die
Leitung gerade belegt ist oder nicht.

Binary Exponential Backoff ist eine Methode, um die Länge der backoff period (bei einer
Kollision) festzulegen. Dabei wird k*c lange gewartet, wobei c eine Konstante ist und k eine
zufällige Zahl in [0,2^i]. i ist die Anzahl an vorherigen gescheiterten Versuchen. 
Durch exponentielle Steigerung der maximalen Wartedauer wird congestion vermieden.

Die jam sequence ist eine Sequenz, die gesendet wird, wenn ein Teilnehmer im Netzwerk eine
Kollision feststellt. Die Länge von Ethernet-Paketen hat ein Minimum, damit garantiert ist,
dass keine Pakete als "fehlerfrei gesendet" markiert werden, weil die jam sequence länger
gebraucht hat, um zum Sender zu kommen, als das Paket zum Senden gedauert hat.
]])





print([[

#######
# CRC #
#######

message is 11001110
poly is 10011
crc length is 4
]])

-- for printing binary representation of integers
oct2bin = {
    ['0'] = '000',
    ['1'] = '001',
    ['2'] = '010',
    ['3'] = '011',
    ['4'] = '100',
    ['5'] = '101',
    ['6'] = '110',
    ['7'] = '111'
}
function getOct2bin(a) return oct2bin[a] end
function convertBin(n)
    local s = string.format('%04o', n)
    s = s:gsub('.', getOct2bin)
    return s
end

-- greatest power of 2 smaller than n
function maxexp(n)
  local e = 0
  while n > 0 do
    n = n // 2
    e = e + 1
  end
  return e
end


function crc(message, poly)
  original = tonumber(message, 2) << #poly - 1
  offset = #poly - 1
  poly = tonumber(poly, 2)

  result = original

  repeat
    print("message", convertBin(result))

    current_poly = poly

    -- align result and poly for division
    if maxexp(result) > maxexp(current_poly) then
      while maxexp(result) > maxexp(current_poly) do
        current_poly = current_poly << 1
      end
    else
      while maxexp(result) < maxexp(current_poly) do
        current_poly = current_poly >> 1
      end
    end

    result = result ~ current_poly -- xor

    print("poly", convertBin(current_poly))
    print("result", convertBin(result), "\n")
  until result < (2 << offset)

  result = result + original
  print("final:", convertBin(result))
  return result
end


print("computing crc:\n")
crc("11001110", "10011")


print("\n\nzeitaufwand: ~7h")

