# Altfragenkatalog zu Einführung Kryptographie und IT-Sicherheit

Salzburg

06.07.21

## 1. Cryptanalysis: Kinds of attacks

Diskutieren sie die verschiedenen Kategorien des “Brechens” eines Verschlüsselungs-
Algorithmus. Zu welcher Kategorie gehört eine “timing-Attacke” ? Inwiefern ist die
brute-force Attacke eine Schranke für die Sicherheit eines Algorithmus ? (2 Punkte)

---

Die grundlegenden Kategorien unterscheiden sich darin, wieviel Information zur Verfügung steht.

* Es gibt folgende Hauptkategorien:
  * Ciphertext only, man hat nur Zugriff auf ein fixes Set von irgendwelchen Ciphertexts
  * Known plaintext, man hat ein Set von irgendwelchen Plain- und Ciphertext Paaren
  * Chosen plaintext, man hat wieder ein fixes Set von Paaren, aber man konnte deren Plaintext beliebig wählen
  * Adaptive chosen plaintext, ist eine chosen plaintext Attacke mit der Möglichkeit, neue Paare beliebig zu erzeugen.
* Und drei kleinere:
  * Chosen ciphertext, man hat die Möglichkeit, beliebige Ciphertexte zu entschlüsseln. Hier sucht man dann den Key.
  * Rubberhose cryptanalysis: Bestechung, Folter, Drohungen. In der Praxis am Effektivsten.
  * Brute Force: Alle Möglichkeiten werden einfach ausprobiert.

Eine Timing-Attacke nutzt aus, dass die Execution Time von einem Algorithmus Informationen liefern kann. Das ist ein recht generelles Prinzip und kann eigentlich in vielen Szenarien angewendet werden, setzt aber wiederholten Zugriff auf den/einen Algorithmus voraus, um effektiv zu sein. Daher fällt es eher in die Kategorien adaptive chosen plaintext und chosen ciphertext.

Brute-Force ist eine obere Schranke für den Aufwand einer Attacke (wenn der Algorithmus bekannt ist), weil Brute Force grundsätzlich immer erfolgreich sein wird, bloß ist die Frage, wie lange das erwartet dauern wird.



## 3. Kerckhoff'sches Prinzip

Formulieren und begründen (mindestens 2 Gründe) sie das Kerckhoff’sche Prinzip
und geben sie Beispiele aus der Realität für desaströse Verstösse und deren 
Konsequenzen. (2 Punkte)

---

Kerckhoff'sches Prinzip: Die Sicherheit eines kryptografischen Systems sollte in der Geheimhaltung des Keys liegen, nicht in der Geheimhaltung des Systems.

Probleme mit diesem Security by Obscurity Verfahren:

* Das Verfahren kann nicht standardisiert werden.
* Es ist leichter, einen Key geheim zu halten, als ein komplettes Verfahren.
* Das gesamte Verfahren muss geändert werden, wenn Information leakt.
* Obscurity bedeutet zwar zusätzliche Sicherheit, aber tendenziell kommen die Verfahren irgendwann doch ans Licht, und geheim gehaltene Verfahren können per Definition keine Qualitätskontrolle durch die kryptografische Community durchlaufen. Daher ist man langfristig tatsächlich eher unsicherer unterwegs als wenn man ein etabliertes bekanntes Verfahren nimmt.

Beispiele: 

* GSM's A5/1 stream cipher was developed in secret, got leaked, and a series of vulnerabilities found.
* The CSS encryption system for DVDs was kept secret and made useless within three years.



## 4. Attacken gegen kryptografische Protokolle

Welche 4 verschiedenen Attacken gegen kryptographische Protokolle wurden be-
sprochen ? Erklären sie kurz wer diese Attacken durchführt. (2 Punkte)

---

* Attacks conducted by outsiders:
  * **Passive attacks**:
    * The procedure of the protocol itself is not affected
    * That means we're just observing the protocol and collecting more information.
    * With this we're limited to [ciphertext-only attack](cryptanalysis)
  * **Active attacks**:
    * We are actually altering the protocol to our advantage
    * e.g. changing, deleting or adding messages, impersonating someone, interrupting communications
* Attacks conducted *by the participants*:
  * **passive cheating**
    * following the protocol but trying to obtain more information than we should
    * in practice, protocols are hardened towards this kind of attack.
  * **active cheating**
    * altering the protocol
    * We can look for (or corrupt) information, degrade performance, fake authorization 
    * In general, it's hard to maintain a protocol if most of the participants are active cheaters




## 5. Dictionary attacks; salting

Erklären sie die Dictionary-Attacke gegen ein Authentifizierungssystem und warum
die Verwendung von SALT (was ist das ?) diese Attacke schwieriger macht. Was
wird bei der Verwendung von SALT beim PWD gespeichert ? Warum ist es also
wichtig starke PWDs zu verwenden ? (2 Punkte)

---

Against general cryptographic systems:

* pregenerate all ciphertexts for short messages, you can then match intercepted messages against that and see if something matches.
  * Work against this by adding random padding to short messages.

Against [authentication](authentication) systems:

* Basically, we have a big list of passwords and their precomputed hashes, and we check whether that list has the hash we're looking for.
  * e.g. rainbow tables, or Feldmeier/Karn's list
* Particularly strong against common passwords, which are often used in practice

Salting to prevent this:

* When creating a password, a random string (the **salt**) is appended to the password and stored with the hash. This string can be public, the whole point is that it will change the resulting [hash](hash-function).
* This prevents [dictionary attacks](dictionary-attack) since the hash tables only hold the hashes of unsalted passwords. Attackers are back to having to [brute force](brute-force) passwords until they find the right hash.
* Of course one could create hash tables of salted passwords, and people did (Feldmeier/Karn), but for long enough salts the hash tables will get infeasibly large.



## 6. RSA $\gcd(m,n)=1$ Wahrscheinlichkeit

Berechnen sie die Wahrscheinlichkeit (und erklären sie ihre Berechnungsschritte)
dass ein zufälliger Nachrichtenblock $M_i$ nicht relativ prim zum RSA-Modul $n$ ist. (2
Punkte)

---

In RSA, if the number to be encrypted ($m$) isn't relatively prime to the modulus $n$ then $\gcd(m,n)$ is per definition either $p$ or $q$ since $n=pq$. That is, $\gcd(m,n)\neq1\implies \gcd(m,n)\in\{p,q\}$. So if the LHS holds, attackers already know either $p$ or $q$, meaning factoring $n$ is now a linear time operation. Thus, the decryption key can easily be found.

In this case the only aid is using a different $m$, however the probability of this happening is only $1-\frac{\phi(n)}{n}$ where $\phi$ is Euler's [totient](totient) function. $\phi(n)$ is small if $p$ and $q$ both are large:

* Since the prime [factorization](factorization) of $n$ is $pq$, only multiples of $p$ and $q$ have a $\gcd$ with $n$ that isn't 1. 
* If for example $p=2$ then there's going to be a lot of multiples of $p$ smaller than $n$ and we have a problem. 
* If both $p$ and $q$ are large, then there won't be that many multiples.
* So: the probability is inversely proportional to $|\min(p,q)|$; specifically $1-\frac{\phi(n)}{n}=\frac{1}{p}+\frac{1}{q}$



## 7. RSA exponent switching attack

Im Zusammenhang mit RSA wurde eine Attacke gegen Verschlüsselung und Sig-
nierung besprochen, bei der die Grundidee ist, dass ein Angreifer einen passend
gewählten neuen öffentlichen Exponenten wählt und publiziert um die Attacke ziel-
gerichtet durchführen zu können. Erklären sie diese Attacke und geeignete Gegen-
massnahmen. (2 Punkte)

---

Say Alice encrypts a message $m$ with Bob's public key $e_B$: $m^{e_B} \mod n_B$

Then she signs it with her private key $d_A$: $(m^{e_B} \mod n_B)^{d_A} \mod n_A$

Now Bob wants to claim that Alice actually sent the message $m'$. Bob knows the factorization of $n_B$ because it's his modulus, so he can feasibly compute the [discrete log](discrete-log) for it! He finds an $x$ such that $m'^x = m \mod n_B$, then re-publishes his public exponent as $xe_B$.

The message from Alice then turns into $(m^{xe_B} \mod n_B)^{d_A} \mod n_A = (m'^{e_B} \mod n_B) \mod n_A$.

Of course this hinges on Alice not being able to prove that Bob changed his key, and it can also be prevented by having everyone use a fixed encryption exponent.


## 8. El Gamal Signatur

Erklären sie die Funktionsweise und beweisen sie schrittweise die Gültigkeit der dig-
italen Signatur nach El Gamal. Benennen sie ausserdem je einen Vor- und Nachteil
verglichen mit RSA-basierten digitalen Signturen. (2 Punkte)

---

![ElGamal Setup](zk/egs.png)
![ElGamal Signing](zk/egsi.png)
![ElGamal Verification](zk/egsv.png)




## 9. CBC und ECB

Erklären sie die Funktionsweise des Cipher-block Chaining Modes für Blockcipher,
Vor- und Nachteile gegenüber Electronic Codebook Mode und erklären sie genau
warum es zur beschriebenen Fehlerausbreitung bei Ciphertextfehlern kommt (und
auch warum diese “self recovering” ist). (2 Punkte)

---

Man bezieht in CBC den vorherigen Ciphertext-Block auch in die Verschlüsselung des nächsten Blocks ein; i.d.R. via [XOR](XOR):

* Encryption mit Key $K$: $C_i = E_K(P_i \oplus C_{i-1})$
* Decryption: $P_i = C_{i-1} \oplus D_K(C_i)$

Das Ganze funktioniert, da $C_{i-1} \oplus D_K(C_i) = C_{i-1} \oplus D_K(E_K(P_i \oplus C_{i-1})) = C_{i-1} \oplus P_i \oplus C_{i-1} = P_i$.

* Pro
  * keine Codebook-Attacken möglich (z.B. lässt sich nicht mehr erahnen, was ein File Header oder ein "Mit freundlichen Grüßen" sein könnte)
  * kein Block Replay möglich (z.B. kann man nicht immer wieder dieselbe Überweisung über 100 Euro einspielen)
* Con
  * Nicht parallelisierbar 
  * Fehler propagieren sich in zwei Blöcke, nicht nur einen

### Error propagation

Beispielhaft betrachten wir Fehlerpropagation bei Anwendung von [DES](DES) im CBC-Modus, aber die Überlegungen gelten für alle Cipher mit denselben Propagations-Eigenschaften wie DES.

* Bitflip im Plaintext: 
  * ändert den gesamten Ciphertext
  * aber lässt sich ohne Probleme wieder entschlüsseln (nur das eine Bit bleibt falsch).
* Bitflip im Ciphertext: 
  * Propagiert sich nur in 2 Plaintext-Blöcke:
    * Sei $C_i$ der Block mit dem Fehler.
    * $P_i$ wird gänzlich unbrauchbar, da jedes Output Bit von jedem Input Bit abhängt
    * $C_{i+1}$ hat genau einen Bitflip-Fehler:
      * wir berechnen beim Entschlüsseln ja $C_i\oplus D_K(C_{i+1})$ 
      * d.h. $C_i$ mit dem Bitflip wird mit dem Entschlüsselungsergebnis von $C_{i+1}$ XORed
      * der eine Bitflip aus $C_i$ landet also auch an genau derselben Stelle in $P_{i+1}$.
    * Block $C_{i+2}$ hat keine Fehler mehr, da er nicht mehr von $C_i$ abhängt: 
      * wir berechnen $C_{i+1}\oplus D_K(C_{i+2})$
      * Also nur $C_{i+1}$ fließt noch ins Ergebnis ein.
    * Selbiges gilt für weitere Blöcke danach.
    * Visuell:
      * ![DES decryption](zk/desd.png)



## 10. Diffie-Hellman

Erklären Sie anhand eines einfachen Protokolls den Diffie-Hellman Key-exchange
Algorithmus. Wählen Sie n = 7 und rechnen Sie ein konkretes Beispiel durch. Was
ist eine Primitivwurzel und warum ist deren Verwendung hier wichtig ? (2 Punkte)

---

### Setup

Choose some $g$ and $n$ such that $g$ is [primitive](primitive) mod $n$. 

* $g$ and $n$ can be public and shared. 
* $g$ should be a primitive of $n$ because otherwise the keyspace is limited to just the subset of $\mathbb{Z}_n$ that $g$ can produce.
  * At least, that should be a large subset.


### Protocol

* Alice chooses some large integer $x$ and sends Bob $X = g^x\mod n$
* Bob chooses some large integer $y$ and sends Bob $Y = g^y\mod n$
* Alice computes the key $k = Y^x$
* Bob computes the key $k = X^y$

![Example.](zk/dhe.png)



## 12. Was ist multiple key public key cryptography und wie funktioniert das ? Für welche
Anwendungsszenarien ist das von Vorteil (und welche konkreten Nachteile klassischer
Ansätze werden dabei vermieden) ? (2 Punkte)2



## 13. Erklären Sie wie die Methode “counting coincidences” verwendet werden kann um
die key-length bei Verschlüsselung mit XOR und einem kurzen Schüssel zu ermit-
teln. Begründen sie auch warum diese Methode funktioniert. Geben sie ein Beispiel
für eine Methode der Entschlüsselung wenn die Schlüssellänge ermittelt wurde. (2
Punkte)



## 14. Erklären die das Konzept von randomisierter Verschüsselung und wie dies bei Sig-
naturen mit El Gamal angewendet wird. Was ist der Vorteil von RSA gegenüber
dem Einsatz von El Gamal bei digitalen Signaturen ? (2 Punkte)



## 15. Beschreiben sie detailliert eine der drei besprochenen Szenarien die darlegen, warum
RSA nie benutzt werden darf um ein “zufällig” wirkendes Dokument zu signieren
(chosen ciphertext Attacke). Erklären sie weiters (konkret anhand des gewählten
Szenarios), was die Anwendung einer Hash-funktion bei der Signierung ändert (und
ob das überhaupt etwas ändert). (2 Punkte)



## 16. Erklären sie die Funktionsweise der S-Box Substitution in DES und vergleichen sie
deren Funktionsweise mit der in AES verwendeten Substitution. In welcher Hin-
sicht besteht ein grundlegender Unterschied (bezogen auf die Substitution aber auch
andere Elemente) zwischen den Designprinzipien von DES und AES ? (2 Punkte)



## 17. Bleibt bei Substitution Ciphers oder Transposition Ciphers das Histogramm des Ci-
phertextes unverändert verglichen mit dem Histogramm des Plaintext ? Begründen
sie ihre Antwort ! Welche Histogramm-basierte Attacke gibt es gegen Verschlüsselung
(von Texten) mit Substitution Ciphers ? (2 Punkte)



## 19. Erklären sie die Common Modulus Attacke gegen RSA und wie sie verhindert werden
kann. (2 Punkte).



## 20. Zum Verständnis von RSA: gegeben sind als public key n = 21 und e = 5. Sie
fangen einen Ciphertext c = 2 ab. Ermitteln Sie durch eine Faktorisierungs-Attacke
den dazugehörigen Plaintext und erklären Sie die Rolle der Faktorisierung bei Ihrer
Attacke. (2 Punkte)



## 21. Erklären Sie die Grundidee der Quantenkryptographie und warum es beim beschriebe-
nen Verfahren möglich ist zu erkennen ob ein Lauscher am Kanal war (2 Punkte).



## 22. Erklären Sie welche Bereiche der IP Pakete im Transport Mode beim ESP Pro-
tokoll von IP Secure authentifiziert bzw. verschlüsselt werden. Was ist Tunnel und
Transport Mode ? Was ist der Unterschied zwischen ESP und AH ? (2 Punkte)



## 23. Welches Problem wird bei SET durch sog. duale Signaturen gelöst und wie funk-
tioniert das (z.B. bei der Bank wenn die Zahlungsinformation entschlüsselt wird) ?
(2 Punkte)



## 24. Wie funktioniert die Geburtstagsattacke gegen one-way Hash functions ? Worauf
beruht sie ? Abhilfe ? (2 Punkte)3



## 25. Was ist eine Zero-Knowledge Protokoll und was ist der Unterschied zu z.B. klassis-
chen Authentifizierungsprotokollen ? Beschreiben sie detailliert die Funktionsweise
des Feige-Fiat Schamir Identifikations Schemas und erklären sie die zero-knowledge
Eigenschaft. (3 Punkte)



## 26. Erklären Sie warum bei OTP Verschüsselung ein OTP nur 1x als keystream verwen-
det werden darf. Welche Attacke wird ermöglicht wenn noch zusätzlich einer von
zwei involvierten Plaintexten (die mit dem gleichen OTP verschlüsselt wurden) zur
Verfügung steht ? (1 Punkt)



## 27. Erklären Sie die Betriebsarten von Blockciphern (ECM, CBC, CFB, OFB), deren
Vor- und Nachteile und ihr Verhalten bei Ciphertextfehlern bzgl Fehlerausbreitung.
(2 Punkte)



## 28. Was ist die “meet in the middle attack” (wie funktioniert sie) und was ist die Kon-
sequenz ihrer Existenz ? (2 Punkte)



## 29. Wie und warum können symmetrische Block-cipher verwendet werden um one-way
Hash functions daraus zu bauen ? Erklären Sie anhand des Tandem Davies-Mayer
Algorithmus konzeptuell wie unter Verwendung von AES ein sicherer 256 bit Hash
erzeugt werden kann. (2 Punkte)



## 30. Beschreiben Sie ein Verfahren und eine Anwendung von Secret Splitting für 2 und
mehr Personen. Was ist der Unterschied zu Secret Sharing ? (1 Punkt)



## 31. Erklären sie drei “computational infeasible problems” die die Grundlage von public
key Verfahren bilden (auch warum das schwierige Probleme sind) und beschreiben
sie in welcher Form sie bei je einem konkreten Algorithmus vorkommen (also warum
sie konkret die Sicherheitsgrundlage dieser Verfahren bilden). (3 Punkte)



## 32. Welcher Zahl im Restklassensystem Modulo 5 entspricht der Ausdruck $−74 \mod 5$?
Erklären Sie die Berechnungsschritte. Bestimmen sie eine Primitivwurzel (was ist
das?) im Restklassensystem Modulo 5. (2 Punkte)



## 33. Erklären Sie die wesentlichen Bestandteile eines Kerberos Realms und den Ablauf
einer Applikationsanforderung und Durchführung. Wie wird inter-realm Authen-
tifizierung realisiert ? Was sind Nachteile von Kerberos ? (2 Punkte)



## 34. Welches grundsätzliche Problem der public-key Kryptographie muss für DNSSEC
(auch) gelöst werden ? Wie wird das in DNSSEC gelöst ? Warum ist DNS Walking
(was ist das ?) durch die Verwendung von NSEC3 RR nicht mehr möglich ? (2
Punkte)



## 35. Wie und mit welchen kryptographischen Primitiven wird eine digitale Signatur typ-
ischerweise erstellt und wie wird sie verifiziert ? Erklären sie die Rolle der Kompo-
nenten und den Grund für ihre Verwendung. Geben sie konkrete Beispiele, welche
Algorithmen für die verschiedenen Komponenten bei der Signaturerstellung verwen-
det werden können. (2 Punkte)



## 36. Erklären sie was das diskrete Logarithmenproblem ist, bei welchen besprochenen
Verfahren es die Sicherheitsgrundlage bildet und erklären sie intuitiv warum Loga-
rithmieren in Restklassensystemen schwieriger ist als in klassischer Arithmetik. (2
Punkte)4



## 37. Erklären sie die Begriffe “Ciphertext-only Attacke” und “Known-Plaintext Attacke”
und erklären sie genau, welches Wissen und welche Vorraussetzungen man für diese
Attacken braucht. Gegen welche Art von Attacke sind Public-key Verfahren grundsätzlich
immer anfällig und warum ? (2 Punkte)



## 38. Erklären Sie die Grundprinzipien von elliptic curve cryptography, die Vor- und
Nachteile, und die Anwendungsbereiche für die diese Verfahren besonders interessant
sind. (2 Punkte)



## 39. Wie könnte eine Mischung zwischen public key und symmetrischen Verfahren funk-
tionieren (Stichwort hybride Verfahren) ? Warum ist so ein Verfahren sinnvoll ? (1
Punkt)



## 40. Wie funktioniert das SKEY Authentifizierungssystem ? Inwiefern unterscheidet es
sich von den meisten TAN-basierten Systemen beim electronic banking ? (1 Punkt)



## 41. Erklären sie was eine “brute force attack” gegen einen cipher ist und unter welchen
Bedingungen diese nicht erfolgreich anwendbar ist. (1 Punkt)



## 42. Beschreiben sie die Funktionsweise, Funktionalitäten und Komponenten von PGP.
(2 Punkte)



## 43. Was ist die “man in the middle attack” (wie funktioniert sie) und wie kann eine
Abhilfe aussehen ? (2 Punkte)



## 44. Erklären sie die Funktionsweise der Hash-Funktion MD-5. Entspricht diese dem
State-of-the-Art ? Welche sichere Alternativen gibt es ? (2 Punkte)



## 45. Beschreiben sie die Anwendungsbereiche, Funktionsweise, Funktionalitäten und ins-
besondere die beiden “Schichten” von SSL und TLS. (2 Punkte)



## 46. Beschreiben sie die beiden wesentlichen Strategien zum Schlüsselmanagement im
Bereich sichere E-mail und ordnen sie die besprochenen Systeme jeweils einer Strate-
gie zu. (2 Punkte)



## 48. Erklären sie die Grundstruktur und Funktionsweise von AES (was ist das für ein ci-
pher, welche funktionellen Grundkomponenten hat er und was tun diese) und disku-
tieren sie die Unterschiede (und deren Grund) zu DES. (2 Punkte)



## 49. Beweisen sie die Korrektheit der RSA Entschlüsselung für den Fall dass der nu-
merische Nachrichtenblock und der Modul relativ prim sind (unter Verwendung des
“kleinen” Fermat). (2 Punkte)



## 50. Leiten sie die folgende Formel für die Eulersche $\phi$-Funktion her: $\phi(n) = (p−1)(q −1)$
für n = pq und p, q prim. (2 Punkte)



## 51. Beschreiben sie die in der VO besprochene Protokollattacke gegen (automatisierte)
digitale Empfangsbestätigungen (verschlüsselt & signiert) und diskutieren sie deren
Realitätsnähe. (2 Punkte)5



## 52. Erklären sie wie das Knapsack-Problem verwendet werden kann, um ein public-key
Verschlüsselungsverfahren zu realisieren. Warum werden solche Systeme heute nicht
mehr eingesetzt ? (2 Punkte)



## 53. Benennen und erklären sie mindestens zwei weitere zentrale Aufgaben der Kryp-
tographie neben der Entwicklung von Verschlüsselungsverfahren und geben sie je
ein konkretes Beispiel von entwickelten Systemen die diese Aufgaben erledigen . (2
Punkte)



## 54. Erklären sie den Unterschied zwischen Block- und Streamcipher und geben sie je ein
Beispiel. (1 Punkt)



## 55. Erklären sie, warum man XOR-Verschlüsselung mit einem kurzen Key (im Gegensatz
zum one-time Pad) als polyalphabetischen Cipher interpretieren kann. Aus wievielen
Alphabethen besteht dieser polyalphabetische Cipher ? Was nützten diese Erkennt-
nisse bei der Entschlüsselung eines Ciphertexts, der mit XOR-short key verschlüsselt
wurde ? (2 Punkte)



## 56. Berechnen sie für den Modul p = 5 eine digitale Signatur nach El Gamal für
die Nachricht M = 3 nach entsprechender (korrekter) Wahl der zusätzlich dafür
notwendigen Parameter und führen sie auch die Verifikation der Signatur durch. (2
Punkte)



## 57. Erklären sie detailliert das Ausmass der Fehlerausbreitung und warum es zu self-
recovery kommt bei den CFB and CTR Blockcipher Betriebsmodi im Fall eines
vorliegenden Ciphertext Bitfehlers. (2 Punkte)



## 58. Rechnen sie exemplarisch zwei Iterationen des Feige-Fiat Shamir Identification Schemas
durch: p = 2, q = 3, s = 5; eine Iteration mit der Challenge b = 0, die andere mit
b = 1. Führen sie alle notwendigen Überprüfungen und Parameterwahlen durch und
erklären sie warum trotz der übermittelten Daten durch Peggy die zero-knowledge
Eigenschaft erhalten bleibt. (2 Punkte)



## 59. Berechnen sie für den Modul p = 5 den Ciphertext nach El Gamal Verschlüsselung
für die Nachricht M = 3 nach entsprechender (korrekter) Wahl der zusätzlich dafür
notwendigen Parameter. Führen sie auch die Entschlüsselung durch und erklären
sie, was ein Angreifer bei einer Ciphertext only Attacke können müsste um den
Plaintext zu gewinnen (wir nehmen an dass der Angreifer den Parameter K für die
randomisierte Verschlüsselung erhalten hat). (2 Punkte)



## 60. Beschreiben sie die besprochenen CBC triple encryption Varianten. Nennen sie zwei
Gründe, warum sie eine der beiden bevorzugen würden. Diese Verfahren wurden für
DES entwickelt - macht eine Anwendung mit AES ebenfalls Sinn ? Begründen sie
ihre Antwort ! (2 Punkte)



## 61. Erklären Sie die Grundidee der quantenkryptographischen Schlüsselerzeugung und
warum es beim beschriebenen Verfahren möglich ist zu erkennen ob ein Lauscher am
Kanal war (2 Punkte).



## 2. In welchem Verhältnis stehen IKE und IPSec (d.h. welche Aufgaben werden von
welchem Verfahren übernommen) ? Inwiefern gibt es in der Struktur von SSH/TLS
Ähnlichkeiten zu dieser Aufgabenverteilung ? (2 Punkte)



## 11. Wie funktioniert bei GSM Netzen die sichere Kommunikation zwischen Benutzer
und Basisstation ? Welches Problem wird dabei unerfreulicherweise nicht gelöst ?
(2 Punkte)



## 18. Was ist ein Message Authentication Code (MAC) ? Gibt es Unterschiede in der
Funktionalität zur digitalen Signatur, wenn ja, inwiefern ? Beschreiben sie jeweilige
Vor- und Nachteile ! Warum wird bei IPSec zur Authentifizierung ein MAC und
keine digitale Signatur verwendet ? (2 Punkte)



## 47. Erklären sie die Grundfunktionalitäten von DNSSEC (was wird wie abgesichert, was
nicht - im Gegensatz zu IPSec, welche zusätzlichen DNS records gibt es dafür) und
beschreiben sie wie die Problematik der Überprüfung der Korrektheit eines public
domain-keys gelöst wird. (2 Punkte)



