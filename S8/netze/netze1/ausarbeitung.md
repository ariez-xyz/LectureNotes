# NVS Fragenkatalog Ausarbeitung


## 1. Was halten Sie für die beunruhigendste Entwicklung im Internet?

Dass Aufmerksamkeit das wertvollste Gut im Internet ist: da das Internet praktisch ausschliesslich durch Werbung finanziert ist, optimieren Anbieter auf maximale Nutzungsdauer. Weil die Mittel, die Nutzungsdauer am Effektivsten maximieren, tendenziell ungesund fuer den Endnutzer sind, fuehrt das zu negativen Konsequenzen fuer Nutzer, z.B. Social Media Sucht und Instagram's Einfluss auf den Selbstwert von Nutzern usw. Dazu kommt natuerlich Privacy Invasion ebenfalls zwecks Werbung.


## 2. Was ist der Unterschied zwischen Deep Web und Dark Web?

Das Dark Web ist nur durch Tor bzw. onion protocol erreichbar. Das Deep Web sind "versteckte" Seiten, die bloss schwer zu finden, z.B. nicht direkt via Google erreichbar.


## 3. Wo ist Privacy by design/default festgelegt und was bedeutet es?

PbD ist ein Design-Ansatz, bei dem Privacy bereits zur Design-Zeit eine wichtige Rolle spielt. Es gibt 7 Prinzipien:

* Proactive not reactive, preventative not remedial
  * Anticipate risks and protect data *before* issues crop up, don't just take care afterwards.
* Privacy as default
  * Data should be protected automatically
  * The default settings should enable privacy
* Privacy embedded into design
  * Privacy shouldn't just be bolted on afterwards
* Full functionality
  * Don't impair functionality
* End-to-end security
  * The entire lifecycle of your data should be private.
* Visibility and Transparency
  * Basically you should be open source. Users can check and see for themselves that everything is private. 
* User-centric
  * This is a bit general
  * Give options to your user re. data: constent, access.


## 4. Was kann man tun, um Anonymität im Internet zu erreichen?

Fingerprinting vermeiden (kein JS, oder User Agent Spoofing mit sehr verbreiteten user agents und eine Standard-Aufloesung verwenden). Cookies ausstellen. VPN ist nur insoweit hilfreich, wie man dem Anbieter mit seinen ganzen Daten vertraut. uMatrix: Third Party Tracker etc. unterbinden. Sich nicht mit seinen Daten irgendwo anmelden. Öffentliche Hotspots nutzen.


## 5. Welche Verordnung sorgt für den Schutz personenbezogener Daten? Wie?

Die DSGVO. Sie limitiert, unter welchen Umständen Daten gesammelt werden können.

* Datensammeln ist grundsätzlich verboten und muss explizit erlaubt werden.
* Daten dürfen nur gebunden zu spezifischen Zwecken gesammelt werden. Der Zweck und wie die Daten verwendet werden sollen muss ausformuliert werden.
* Datenminimuerung: Unternehmen sollen so wenige Daten wie möglich sammeln.
* Transparenz: Datenverarbeitung soll nachvollziehbar sein. Z.B. sollen die vorliegenden Daten über einen Nutzer auf Anfrage geliefert werden.
* Vertraulichkeit: Die gespeicherten Daten müssen vom Unternehmen geschützt werden. Dies ist eine ausdrückliche Verpflichtung, wenn auch recht subjektiv.


## 6. Was bedeutet die Einführung von DoH?

DNS-Anfragen sind jetzt privat und können nicht mehr abgehört werden. Weiters werden MITM-Attacken erschwert: dank HTTPS kann kein Angreifer mehr ein DNS-Paket abfangen und selber eine Antwort darauf schicken. Es gibt DoH-Implementationen, bei denen kein Server sowohl die Adresse als auch den Inhalt einer Anfrage kennt.


## 7. Welche Probleme enstehen durch Vernetzung?

Soziologische Probleme:

* "Always on" mentality
* Auch psychologische Probleme - z.B. sieht man im Internet die 0.1% von den 0.1%, was zu schlechtem Selbstwert fuehrt


## 8. Wie entsteht und funktioniert der Überwachungskapitalismus?

Menschen ziehen Convenience über Datenschutz vor, dadurch konnten gewisse Firmen extrem machtvoll werden (massive Datensammlung; Daten = Werbung = Geld; Geld = Macht; außerdem auch Daten = Macht).

Die Datenkraken sind so machtvoll, dass sie eine Gefahr für die Demokratie und persönliche Freiheit der Menschen sind. Die Menge an Daten und insbesondere auch Monopolisierung des Informationsflusses gibt Tech Giants extrem viel Macht.


## 9. Was bedeuten G-MAFIA und BAT?

G-Mafia sind US Tech Giants (Google, MS, Amazon, FB, Twitter, ...) . BAT sind China Tech Giants (Baidu Alibaba Tencent). Die Akronyme sind nicht exhaustive, bei G-Mafia gehört Twitter definitiv dazu und die Relevanz von IBM ist heutzutage eher fragwürdig. Die Unterscheidung zwischen den beiden Akronymen liegt daran, dass die G-Mafia die US-Kapitalisten sind und BAT die chinesische Interpretation von Kommunismus.


## 10. Welche Änderungen von Diensten forderte EFF bereits Anfang 2018?



## 11. Was ist ein Hohlleiter?

Hohlleiter sind hohle Metallrohre; entweder elliptisch, rund oder rechteckig; innerhalb des Leiters können besonders hoch frequente Wellen gut übertragen werden. Ab ca. 1GHz Frequenz weisen Koaxialleiter Probleme auf, diese werden immer schlimmer, bis Koaxialleiter ab ca. 36GHz praktisch kaum noch verwendet werden können. Bei Hohlleitern treten diese Probleme nicht auf.

Bei Koaxleitern ist eine Leitung wie ein Zylinder aussen ausgelegt, und innerhalb dessen sitzt die andere Leitung.



## 12. Was sind Ziele von Rechnernetzen?

Wir wollen zuverlässige, günstige und skalierbare Netze. Das grundsätzliche Ziel ist die Kommunikation und alles was Kommunikation ermöglicht (and that's a lot)


## 13. Welche Unterscheidungskriterien gibt es für Netze?

* Aufbau/Struktur/Topologie: wer kann mit wem kommunizieren? Wo entlang muss diese Kommunikation dabei verlaufen?
* Anzahl Teilnehmer
* Umfang (physisch)
* Broadcast? P2P?
* Welche physische Leitung wird verwendet?
* Synchronisiert oder nicht?


## 14. Was sind dynamische Netzzugänge und in welcher Hinsicht sind sie das?




## 15. Was sind asynchrone Übertragungsarten und in welcher Hinsicht sind sie das?

Es gibt keinen globalen Taktgeber, an den sich alle Teilnehmer halten. Der Takt wird bei der Signalübertragung ausgemacht. Bei GPS wird der Takt zwar im Signal mitgeliefert, aber hält sich dennoch an die Atomuhren.


## 16. Was unterscheidet synchrone von asynchroner Übertragung?

Die Teilnehmer sind synchron, wenn sie beide im exakt gleichen, extern angegebenen Takt kommunizieren. Asynchrone Kommunikation muss natuerlich auch getaktet werden, hier geschieht das aber direkt mit dem Signal.


## 17. Welche Entwurfsaspekte sind für Netzwerke entscheidend?

* Zuverlaessigkeit
* Fehlerfreiheit
* Resilienz gegen ausfallende Knoten (historisch wegen Cold War, central phone offices, etc.)
* Kosten
* Reichweite/Ausdehnung


## 18. Skizzieren Sie den Zusammenhang zwischen Schicht, Schnittstelle, Dienst und Protokoll

* Schnittstellen sitzen zwischen Schichten
* Schichten sind Abstraktionsebenen, die ein bestimmtes Problem lösen sollen
* Protokolle werden verwendet, um innerhalb einer Schicht die Kommunikation zu regeln
* Dienste sind die Aufgaben, die eine Schicht für die darüberliegende Schicht erledigen kann


## 19. Beschreiben Sie die grundlegenden Dienstprimitiven.

* **Request**: eine Request gesendet von layer $N$, irgendetwas zu tun. Die Request beinhaltet nötige Parameter.
* **Confirm**: Bestätigung auf eine Request, gesendet von layer $N-1$
* **Indication**: Information, dass irgendein Event eingetreten ist; gesendet von layer $N-1$
* **Response**: Antwort auf ein Event, gesendet von layer $N$



## 20. Skizzieren Sie das OSI-Modell und geben Sie pro Schicht eine/die wesentliche Funktion an.

1. [physical layer](physical-layer) Physische Übertragung der Daten von einen Ort an den Anderen
2. [data link layer](data-link-layer) Protokoll, um Daten fehlerfrei über die physical layer zu übertagen
3. [network layer](network-layer) Festlegung der Route, die die Daten nehmen sollten
4. [transport layer](transport-layer) Protokoll zur Übertragung zwischen network hosts (Fehlerbehebung, Aufteilung in Chunks)
5. [session layer](session-layer) Opening and closing communication between hosts
6. [presentation layer](presentation-layer) Make data presentable for application layer
7. [application layer](application-layer) Protocols for user data (NOT the apps itself, but [HTTP](HTTP), SMTP, FTP, etc.)



## 21. Benennen und erklären Sie die wichtigsten Aufgaben der Transportschicht.

Takes data from the users above and makes sure that it arrives correctly on the other end. It abstracts away the network layer, meaning that different networks can be dealt with transparently. It provides some redundancy in correcting for errors in the network layer.


## 22. Benennen und erklären Sie die wichtigsten Aufgaben der Sitzungsschicht.



## 23. Benennen und erklären Sie die wichtigsten Aufgaben der Anwendungsschicht.

Die Anwendungsschicht bietet Protokolle, die dann von den Endanwendungen genutzt werden koennen, um den Nutzern schließlich Dienste bereitzustellen. Die zuvorigen Schichten kümmern sich rein um den Transport von Daten; jetzt geht es darum, was man mit den Daten anfangen kann. Hierzu geören z.B. HTTP, FTP, SSH, SMTP; aber auch DNS ist Teil dieser Schicht.


## 24. Was ist DCF-77

DCF77 ist ein Langwellensender bei Frankfurt, der Funkuhren mit der Zeit versorgt.

Jede Minute werden 59 Bits übertragen, indem am Ende jeder Sekunde die Amplitude für 200 oder 100ms abgesenkt wird. Am Anfang jeder Minute wird die Amplitude eine Sekunde lang gar nicht abgesenkt.



## 25. Definieren Sie die Begriffe Schnittstelle, Signal, Laufzeit?

* Schnittstellen verbinden unterschiedliche Systeme (w.r.t Mechanik, Kodierung, Elektrik, ...)
* Ein Signal ist ein Zeichen mit einer zuvor bestimmten Bedeutung. Signale können Information übertragen.
* Die Signallaufzeit ist die Zeit, die ein Signal braucht, um vom Sendeort zum Empfangsort` zu kommen.



## 26. Was unterscheidet Hub, Switch, Router?

* Ein Hub hat mehrere Ports und klont ankommende Frames an alle Ports (**broadcasting**)
  * Hubs arbeiten auf der physical layer?
  * Hubs sind "dumm"
* Ein Switch arbeitet auf der data link layer oder sogar auf L3; er kennt die MAC-Adressen der verbundenen Geräte und leitet Frames an den entsprechenden Port weiter, statt einfach zu broadcasten (weniger congestion)
* Ein Router arbeitet auf OSI L3 - anders als die anderen beiden sitzt er zwischen actual networks und kann IP inkl. NAT, DHCP, etc.


## 27. Erklären Sie die wichtigsten Aufgaben/Funktionen der Bitübertragungsschicht?

* Sicherstellen, dass eine 1 ankommt, wenn man eine 1 sendet, und dasselbe mit 0
* Sicherstellen, dass die Daten in der richtigen Reihenfolge ankommen
* Diese Layer ist der primäre Faktor in Latenz, Bandbreite, error rate, ...


## 28. Welche Gremien standardisieren Netzwerke?

* ISO (ANSI, DIN, ...)
* IEEE
* Internet Architecture Board/Internet Engineering Steerign Group/Internet SOCiety
  * Internet Research Task Force
  * Internet Engineering Task Force (die machen die ganzen RFCs)
* ICANN, IANA (domains/DNS)


## 29. Was versteht man unter „Internet of Things“?

Dass Alltagsgegenstände mit dem Internet vernetzt werden sollen und dadurch nützlicher werden sollen. Man stelle sich einen Kühlschrank vor, der automatisch Essen nachbestellt, oder eine fernsteuerbare Kaffeemaschine


## 30. Was besagt die Formel von Nyquist?

$$
C = 2B \log_2 M
$$

This formula gives the *minimum* required data bitrate to represent an analog signal of [bandwith](bandwith) $B$ (in hertz) as a digital signal with $M$ levels



## 31. Was besagt die Formel von Shannon?

$$
C = B \log_2 (1 + \text{SNR})
$$

This is the theoretical *maximum* transfer rate in bits/second over some channel, where $\text{SNR}$ is the signal-to-noise ratio.




## 32. Was kennzeichnet analoge im Gegensatz zu digitalen Signalen?

Digitale Signale haben immer diskrete Zustände.


## 33. Was versteht man unter Rauschen?

Störenergie (natural or man-made), die irgendwie in den Übertragungskanal reingekommen ist, und die das Signal verzerren kann.


## 34. Welche Modulationen verwenden FSK, ASK, PSK?




## 35. Was ist Baud?

Number of symbols per seconds.


## 36. Was ist Rauschen/Fehler/Signal?

* Rauschen: Einflüsse auf das Signal, die aus der Umgebung kommen
* Fehler: Die Differenz zwischen einem berechneten/gemessenen Wert und dem tatsächlichen/theoretischen Wert
* Ein Signal ist ein Zeichen mit einer zuvor bestimmten Bedeutung. Signale können Information übertragen.


## 37. Was versteht man unter Modulation?

Das "Mapping" von einem niederfrequentem Input-Signal auf einen höherfrequenten Bereich. 

Mittels Modulation können unterschiedliche Sprach-/Musiksignale parallel übertragen werden, ohne sich zu stören, weil sie auf unterschiedlichen Wellen laufen. Bei hohen Frequenzen sind auch nur noch kleine (bzw. nicht riesige) Antennen vonnöten.



## 38. Skizzieren Sie kurz drei Modulationsverfahren?

Amplitudenmodulation: die Amplitude des höherfrequenten Signals korrespondiert zu der der Nachricht.

Frequenzmodulation: Änderungen in der Frequenz des hohen Signals kodieren Änderungen in der Amplitude der Nachricht.

Phase Shift keying: die carrier wave hat eine konstante Frequenz, und bei jedem Bit-Übergang wird die Phase des Signals geändert


## 39. Was versteht man unter Multiplexing?

Mehrere Signale auf einem einzelnen Kanal parallel übertragen.


## 40. Skizzieren Sie drei Multiplexing-Verfahren.

* Frequency division: jedes Signal bekommt einen Teil der Frequenz des Kanals
* Time division: jedes Signal bekommt einen "turn", nacheinander
* Wavelength division: z.B. können optische Signale mit unterschiedlichen Wellenlängen über dieselbe Glasfaserleitung übertragen werden, und am Ende mit einem Prisma wieder aufgeteilt werden.


## 41. Was versteht man unter Signalisierung?

* Kontrolle/Steuerung von Kommunikation mittels Signalen
* In networks, die "Metadaten"-Austausche, die stattfinden, bevor user data ausgetauscht werden kann


## 42. Was sind die Unterschiede zwischen Kupfer und Glasfaserkabel?

* Glasfaser hat mehr Bandbreite
* Glasfaser ist fragiler
* Kupfer ist störanfälliger
* Kupfer wiegt mehr
* Glasfaser ist nicht ohne Weiteres abhörbar
* Glasfaser verwendet Photonen, Kupfer verwendet Elektronen
* Kupfer ist billiger


## 43. Was ist Vectoring?




## 44. Was unterscheidet analoger von digitaler Kommunikation?

Digital ist superior - Analog ist störanfällig, verzerrt leichter, verliert leichter Informationen, digital kann höhere Datenraten erreichen


## 45. Worin unterscheiden sich tabellarisch Leitungs- und Paketvermittlung?


|                   | Leitung | Paket     |
|-------------------|---------|-----------|
| Dedizierter Pfad  | y       | n         |
| Bandbreite        | fix     | dynamisch |
| Belegung          | fix     | variabel  |
| Buffering         | n       | ja        |
| Route             | fix     | dynamisch |
| Verbindungsaufbau | y       | n         |
| Verrechnung       | Zeit    | Menge     |


## 46. Nennen Sie drei Übertragungsmedien und ihre wesentlichen Charakteristika.

* EM-Wellen, kabellos, Sender müssen aber stark sein oder die Reichweite ist gering
* Glasfaser, siehe 42
* Kupferkabel, siehe 42


## 47. Was ist das SONET?

Synchronous Optical Network ist eine Technik um Datenströme über Glasfaser zu multiplexen.


## 48. Was sind die Unterschiede zw FDM, TDM und CDM?




## 49. Skizzieren Sie die Funktionsweise von TDM, FDM und WDM.




## 50. Wo lässt sich Infrarot, Mikrowellen und UV-Strahlung auf dem Frequenzband einordnen?

Infrarot        10e12 bis 10e14 Hz
visible light   10e14 bis 10e15 Hz
UV              10e15 bis 10e16 Hz
Mikrowellen     10e8  bis 10e11 Hz


## 51. Was ist SLIP?

SLIP implementiert IP auf serial ports und 


## 52. Welche Leistungsmerkmale hat ISDN?

ISDN is a standard for moving voice and data over telephone lines. ISDN had little adoption because it was rather late, and got replaced with DSL.

ISDN liefert 2x64 Kbit fuer data/voice und 1x16 Kbit fuer signalling metainfo/control information.

ISDN hatte advanced telefonie features: 

* Mehrere Rufnummern pro Anschluss
* Rufnummerübermittlung (Nummer wird dem, den man anruft, angezeigt)
* Subaddressierung (je nach Anrufer kann ein anderes Gerät erreicht werden)
* Konferenz mis zu 3 Leute
* Anklopfen: bei belegter Leitung kann man "anklopfen" und der Angerufene kann abheben, ablehnen, oder ignorieren
* Makeln: man kann zwei aktive Verbindungen haben und zwischen ihnen hin und her wechseln.


## 53. Charakterisieren Sie xDSL.

DSL (digital subscriber line) ist wie [ISDN](ISDN) (und dial-up) ein Standard, der Internetzugang über phone lines ermöglicht. DSL ist relativ schnell und die line ist nicht shared, weder zwischen Haushalten noch zwischen Telefon und Internet. Es wird noch ein DSL-Modem benötigt.

DSL erlaubt Geschwindigkeiten bis zu 350Mbit, je nach Form, gewöhnlich sind ca. 100Mbit das Maximum.

### ADSL
Asymmetric DSL hat viel schnelleren Downlink als Uplink, was es günstiger macht und gut für Haushalte geeignet.

### SDSL
Symmetric.

### VDSL
~3x schneller als ADSL (theoretisch 350K), aber funktioniert nur auf kürzeren Distanzen


## 54. Über welche „Leitungen“ bieten Internetprovider Zugänge an?

* Telefonleitungen (ISDN, DSL)
  * zuverlaessig, schnell, verbreitet (Telefon existiert fast ueberall)
* Mobilfunk
  * portabel, braucht keine speziellen Einrichtungen vor Ort abgesehen von Sendemasten in der Umgebung
  * schnell bis sehr schnell
  * nicht das Stabilste
  * im Falle von LTE: evtl. Auslastungsprobleme
* Glasfaser
  * extrem schnell
  * stabil
  * teuer und nur an wenigen Orten zu haben
* Satelliten
  * braucht Satellitenschuessel
  * Ping (ausser in Starlink's LEO)
  * funktioniert fast ueberall ohne weitere lokale Infrastruktur
  * space debris
* TV-Leitungen (Kabel genannt)
  * auch verbreitet, schnell


## 55. Charakterisieren Sie fünf Internet-Zugangsarten.

siehe 54


## 56. Auf welchen Frequenzbändern senden welche meistgenutzten 802.11 Standards?

Um 2.4 und 5GHz definiert der Standard eine Nummer von Kanaelen innerhalb einer gesetzlichen nutzbaren Frequenz, in der EU z.B. 5.150 bis 5.350 MHz (Kanal 36 bis 64) und 5.470 bis 5.725 MHz (Kanal 100 bis 140).

Man kann sich dann auf eine Kanalbreite festlegen. Je breiter der Kanal, desto wahrscheinlicher sind Ueberlappungen und Stoerungen; aber desto schneller ist auch die theoretische Uebertragungsrate.

* 802.11a
* 802.11b
  * 22MHz Kanalbreite
* 802.11g
  * 20MHz Kanalbreite
* 802.11n
  * 20 oder 40MHz Kanalbreite
* 802.11ac/ax
  * 80 bis 160MHz Kanalbreite



## 57. Was ist Free Space Optics?

Optische Datenübertragung ohne abgeschlossene Übertragungsleitung, z.B. die Infrarotschranken bei manchen Pissoirs


## 58. Wie können Satelliten für Internet-Dienste verwendet werden?

Grundlegend, indem der Endnutzer irgendwie mit dem Satelliten kommuniziert, und der Satellit reicht die Daten (ggf. ueber weitere Satelliten) an eine Basisstation auf der Erde weiter. Der Endnutzer braucht eine Satellitenschuessel (oder andere Form von Empfaenger), um mit den Satelliten kommunizieren zu koennen; und die Satelliten im Weltraum koennen z.B. mit free space optics kommunizieren (im Falle von Starlink).


## 59. Welche Methoden zur Kollissionsvermeidung gibt es?

* CSMA/CD
* Man kann auch seine Topologie so waehlen, dass es keine Kollisionen geben kann; das bringt natuerlich wieder andere Einschraenkungen mit sich
* Token ring:
  * Empty information frames are continuously circulated on the ring.
  * When a computer has a message to send, it seizes the token. The computer will then be able to send the frame.
  * The frame is then examined by each successive workstation. The workstation that identifies itself to be the destination for the message copies it from the frame and changes the token back to 0.
  * When the frame gets back to the originator, it sees that the token has been changed to 0 and that the message has been copied and received. It removes the message from the frame.
  * The frame continues to circulate as an "empty" frame, ready to be taken by a workstation when it has a message to send.



## 60. Wofür steht CSMA/CD? Skizzieren Sie die Funktionsweise von CSMA/CD.

An early method to avoid collisions in half-duplex connections where data can only run in one direction at a time. 

* CS stands for Carrier Sense; sensing whether the wire is currently busy.
* MA stands for Multiple Access; meaning multiple clients can be connected to the network.
* CD stands for Collision Detection. A client does this by checking whether the signal on the wire is the same as the signal being sent.
* There's also a CSMA/CA variant for wired networks.

![A busy network](/Users/ariez/MEGA/Zettelkasten/csma.png)

The basic principle is sensing whether the wire is idle, and only sending packets when it is.

Collisions can still happen if two computers send at exactly the same time, so if that happens both computers will wait a random amount of time. If a client detects a collision, a jamming signal is sent to pause all communications and initiate the random wait.

CSMA/CD doesn't scale well because collisions are expensive, and the number of collisions rises as the number of clients rises. Efficiency ranges between 30% and 70% of theoretical maximum. To prevent certain machines from completely claiming the network, there are limits on packet size.




## 61. Nennen Sie wenigstens fünf 802.X Standards und ihren Einsatzbereich?

The 802 IEEE standards are related to local networks.

* 802.3 CSMACD ethernet
* 802.5 token ring networks (see 59.)
* 802.11 WiFi
* 802.15 Personal Area Networks
  * 802.15.1 Bluetooth
  * 802.15.4 Zigbee, a low-power, low-bandwidth radio-based PAN for home automation and such.



## 62. Was versteht man unter statischer bzw. dynamischer Kanalzuordnung?

### Static allocation

Use some [multiplexing](multiplex) scheme to cut the channel up into $n$ pieces with $n$ fixed.

* Works fine for a relatively stable number of users with constant traffic
* For example, radio stations
* Doesn't work fine if:
  * Some users don't transmit much, wasting bandwidth others could use
  * the actual number of users is greater than $n$ - some will be denied access
  * the actual number of users is much less than $n$ - again wasted bandwidth

### Dynamic allocation

Dynamically allocating channels resolves the issues named with [#Static allocation](#Static allocation).

Generally, this requires some features such as stations' ability to detect collisions (example: via [CSMACD](CSMACD)), and possibly the ability to sense whether the channel is currently in use (called **carrier sense**).

Note the former is not necessarily required as you can design your protocols such that collisions are impossible ([token ring](token-ring), see also [802](802)).




## 63. Was ist eine Bus-Topologie?

Set of hosts connected via one single common bus in half [duplex](duplex). A host is called a **station** in this context. Some form of collision detection is needed ([CSMACD](CSMACD))

It's simple to set up and maintain but has suboptimal performance and failure resistance.



## 64. Welchen Vorteil haben Ring-Netze?

* Can handle collision, [CSMACD](CSMACD) or a dedicated host for managing the network are not needed
* Works better than bus under heavy traffic
* Guaranteed equal access to resources



## 65. Was bezeichnet der Begriff Byte-Stuffing und welchen Zweck hat das Verfahren?

The data link layer is probably going to be further chopping up the packets it receives into frames.

To delimit frames, one can use a fixed flag byte that's inserted at the beginning and the end of the frame:

![A delimited frame](/Users/ariez/MEGA/Zettelkasten/bstf.png)

Now, what if the `FLAG` byte appears in the payload? The solution is the same as [unix](unix)'s escape sequences. We define an escape sequence, and if `FLAG` appears in data, escape it; further if the escape byte appears that's escaped too:

![Escaping data sequences](/Users/ariez/MEGA/Zettelkasten/bstfn.png)

(You can't just use the `FLAG` byte as escape sequence itself by putting it twice if it appears within data, because that'd be the same as a frame ending and a new one starting.)


## 66. Welche Aufgaben hat die Leitungsschicht?

Daten in Frames aufteilen, 


## 67. Welche Aufgaben hat die Sicherungsschicht?

* Frame delimiting and recognition
* Addressing of destination stations (both as individual stations and as groups of stations)
* Protection against errors, generally by means of generating and checking frame check sequences
* Control of access to the physical transmission medium
  * this is an issue with shared physical layers, e.g. a shared optical fiber line for a block of homes


## 68. Was sollte bei einem Protokoll des Data Link Layer vereinbart werden?

* Wie Pakete in Frames aufgeteilt werden
  * Paketlaenge
  * Sequenznummern
  * Art/Typ des Frames (Daten, Ack, Nak)
* Interfacing mit den Layers drunter und drueber
  * Pakete/Frames und Events holen und senden
* Wie die Frames "delimited" sind
* Error correction
* Medium access control


## 69. Skizzieren Sie ein Stop/Wait Protokoll auf Sender-/Empfänger-Seite.

This solves the buffer overflow problem (aka fast sender, slow receiver problem) but still ignores error correction. The receiver provides ACKs to the sender and the sender won't send the next frame without the previous ACK (this strategy is called **stop and wait**.

It's a [simplex](simplex) protocol because data only flows in one direction, but frames must be able to travel in both directions.

Sender:

```c
frame s;
packet buffer;
event_type event;

while (true) {
  from_network_layer(&buffer);
  s.info = buffer;
  to_physical_layer(&s);
  wait_for_event(&event);       // wait for some frame to arrive (we can ignore its contents, it's always going to be an ACK)
}
```

Receiver:

```c
frame r, s;
event_type event;

while (true) {
  wait_for_event(&event);
  from_physical_layer(&r);
  to_network_layer(&r.info);
  to_physical_layer(&s);        // send some frame to sender (contents do not matter)
}
```



## 70. Skizzieren Sie ein ARQ Protokoll auf Sender-/Empfänger-Seite.


Now let's solve error correction. You pass along a checksum and stop-and-wait until the receiver ACKs the frame. After some timeout, assume the frame was lost and send it again.

However, we also need seq numbers here. If we didn't have them, a frame might get transmitted twice if the frame arrives correctly but its corresponding ACK is lost and the sender mistakenly re-sends it.

Note though that we don't need a huge amount of seq numbers, just a seq bit is enough, since we're only ever deciding between whether to send frame $n$ or $n+1$.

![Collini's Ueberblick](/Users/ariez/MEGA/Zettelkasten/arq.png)



## 71. Nennen Sie wenigstens fünf Kanalzugriffsverfahren?

* ALOHA
  * Clients are communicating with some central entity. They just send whenever they have data, of course there will be collisions and data will be jumbled. The central computer solves the issue by rebroadcasting all frames that it received, so stations will know whether their frame got through. If not, they will wait for a random amount of time.
* sALOHA
* CSMA
* CSMACD
* token ring


## 72. Wozu dient ein Schiebefenster und wie kann man die erforderliche Schiebefenstergröße berechnen?

Sliding windows solve issues that come up with ACKs if you have huge latency. Imagine a 250ms roundtrip time: if you wait for an ACK after each sent packet, you would have extremely low channel utilization.

Choosing $w$ to maximize channel utilization is a function of the bandwidth $b$, delay $d$, payload size $s$: 

* at any time, there will be $bd$ bits in transmission *in one direction*
* dividing by $s$ gives us the number of frames in transmission: $f = \frac{bd}{s}$
* Then, the minimum sliding window size is $w = 2\frac{bd}{s} + 1$ (Hin- und Rueckweg plus eins fuer outstanding ACK)


## 73. Wozu dienen Duplicate ACKs?

In TCP, if an out-of-sequence packet arrives at the receiver, he will send an ACK for the packet number it's waiting for. TCP specifies that if the sender gets three such duplicate ACKs, we can assume the packet in question was lost and should be resent. This is much faster than timeouts which have to be conservative by nature.



## 74. Wozu dienen Cummulative ACKs?

Ein ACK gilt als ACK auch fuer alle kleineren seq's. Das Ziel ist weniger ACKs senden zu muessen



## 75. Wozu dienen NAKs?

Implementation der selective repeat strategy fuer pipelined transmission.

(Wenn ein frame gedroppt wird, wird nur dieser eine Frame neu gesendet, nicht alle seit diesem Frame)


## 76. Was bedeutet bei Sequenznummern „verbotene Zone“?




## 77. Skizzieren Sie den PPP Header

* The frame byte is `0x7E`
* If that appears within the frame, it's escaped with `0x7D` plus XOR'd with `0x20`
  * That means if we see a `0x7E` it's guaranteed the end of the frame
  * The escape byte itself is escaped this way, too.

A frame then looks like this:

![ppp png](/Users/ariez/MEGA/Zettelkasten/ppp.png)

* Flag is constant
* Address is constant
  * PPP's header format follows that of HDLC, another earlier protocol; there was no need to reinvent the wheel but PPP is always adressing all stations so this is hardcoded `0xFF`.
* Control is for frame numbering, but defaults to `0x03`
* The protocol byte(s) tells what kind of [network layer](network-layer) packet sits in the payload (e.g. [ip4](ip4), or PPP sub-protocols [#LCP](#LCP) and [#NCP](#NCP))
* The payload field is variable-length with the maximum negotiated by [#LCP](#LCP).
* 2- or 4-byte [CRC](CRC) checksum can be negotiated as well
* 



## 78. Was ist dynamische Kanalzuordnung?

Dynamically allocating channels resolves the issues named with [#Static allocation](#Static allocation). These would be:

* Some users don't transmit much, wasting bandwidth others could use
* the actual number of users is greater than $n$ - some will be denied access
* the actual number of users is much less than $n$ - again wasted bandwidth

Generally, this requires some features such as stations' ability to detect collisions, and possibly the ability to sense whether the channel is currently in use (called **carrier sense**).

Note the former is not necessarily required as you can design your protocols such that collisions are impossible ([token ring](token-ring), see also [802](802)).

Examples:

* [CSMA, CSMACD](CSMACD)
* [ALOHA](ALOHA), sALOHA
* [token ring](token-ring)



## 79. Erklären Sie ALOHA. Was ist slotted ALOHA und warum wurde es entwickelt?


A 70s system for solving [channel allocation](channel-allocation).

Clients are communicating with some central entity. They just send whenever they have data, of course there will be collisions and data will be jumbled. The central computer solves the issue by rebroadcasting all frames that it received, so stations will know whether their frame got through. If not, they will wait for a random amount of time.

### sALOHA

Divide time into discrete slots, so machines don't send at random times but always within a slot, which improves the channel utilization by reducing the amount of time that collisions will last for.



## 80. Welche Rahmenerkennungsverfahren kennen Sie? Erklären Sie ein Verfahren.

* A length field in the header
* Or [byte stuffing](byte-stuffing):

To delimit frames, one can use a fixed flag byte that's inserted at the beginning and the end of the frame:

![A delimited frame](/Users/ariez/MEGA/Zettelkasten/bstf.png)

Now, what if the `FLAG` byte appears in the payload? The solution is the same as [unix](unix)'s escape sequences. We define an escape sequence, and if `FLAG` appears in data, escape it; further if the escape byte appears that's escaped too:

![Escaping data sequences](/Users/ariez/MEGA/Zettelkasten/bstfn.png)

(You can't just use the `FLAG` byte as escape sequence itself by putting it twice if it appears within data, because that'd be the same as a frame ending and a new one starting.)



## 81. Skizzieren Sie das 802.3 Rahmenformat.

![An ethernet frame (as standardized by DIX or IEEE)](/Users/ariez/MEGA/Zettelkasten/eth.png)

* Preamble aka frame delimiter
* Dest and source addrs
  * the dest can also be a group address (multicast) if it starts with a 1
  * or broadcast if it's all 1s
* Length
  * The type field tells the receiver what to do with this frame
  * IEEE wanted this field to carry length: 
    * in the DIX standard (the one officially named Ethernet) the length was determined by looking inside the data - a layering violation
    * But then they needed another header field for the type
    * Also, DIX was way more prevalent already, so they pedaled back in the end
* At minimum 64 and up to 1500 data bytes
  * The maximum is there because NICs need RAM which was expensive back then
  * The minimum is there for [CSMACD](CSMACD)
    * A sender shouldn't have finished sending his packet before the collision jamming signal arrives back. To prevent this from happening with two machines at maximum distance we need this minimum frame size.



## 82. Worin unterscheidet sich das 802.3 und das Ethernet-Rahmenformat?

Im Type/Length field.

The type field of an ethernet frame holds an EtherType. That describes the type of the data, such as:

* [ip4](ip4)
* [ARP](ARP)
* Wake on LAN
* ...
* Full list: [here](http://192.168.8.117:8000/archive/1623920955.497958/index.html)

However, in the DIX standard (the one officially named Ethernet) the length was determined by looking inside the data - a layering violation. So 802.3 added a separate mechanism for telling the type, and reused this as length field.


## 82. Charakterisieren Sie den IEEE 802.15 Standards kennen Sie?

* 802.15 [PAN](network-types)s, for example:
  * 802.15.1 [bluetooth](bluetooth)
  * 802.15.4 [zigbee](zigbee)

Zigbee: A low-power, low-bandwidth radio-based [PAN](network-types) for home automation and such.


## 83. Wie sieht der Ethernet Rahmen aus und was bedeuten die Felder?

* Preamble aka frame delimiter
* Dest and source addrs
  * the dest can also be a group address (multicast) if it starts with a 1
  * or broadcast if it's all 1s
* Type
  * The type field tells the receiver what to do with this frame
  * IEEE wanted this field to carry length: 
    * in the DIX standard (the one officially named Ethernet) the length was determined by looking inside the data - a layering violation
    * But then they needed another header field for the type
    * Also, DIX was way more prevalent already, so they pedaled back in the end
* At minimum 64 and up to 1500 data bytes
  * The maximum is there because NICs need RAM which was expensive back then
  * The minimum is there for [CSMACD](CSMACD)
    * A sender shouldn't have finished sending his packet before the collision jamming signal arrives back. To prevent this from happening with two machines at maximum distance we need this minimum frame size.



## 84. Warum müssen MAC Adressen eindeutig sein? Wie erreicht man das?

Ethernet implements [MAC](MAC) via *globally unique* IDs (MAC adresses). These are 48-bit/6-byte sequences handed out to manufacturers in 24-bit blocks by IEEE. The manufacturer hardcodes its initial 3 bytes and a unique sequence of the last 3 bytes into each NIC.

These IDs are globally unique so that it can be guaranteed that any Ethernet NIC can address any other Ethernet NIC just by this 48-bit sequence.



## 85. Wie ist die MAC-Adresse aufgebaut?

6 bytes. The first three bytes are per-manufacturer assigned by IEEE (MACs are distributed in three-byte blocks), the last three are assigned by the manufacturers per-card.


## 86. Wie sehen die ARP-Nachrichten aus, wenn Rechner A mit IPv4 Adresse A und MAC Adresse X von Rechner B mit IPv4 Adresse B und MAC Adresse Y benötigt?

Request:

FF:FF:FF:FF:FF:FF
X
EtherType
ARP header...
X
A
00:00:00:00:00:00
B

Response:

X
Y
EtherType
ARP header...
X
A
Y
B


## 87. Was erreicht man mit „bridging“?

Mehrere eigentlich separate LANs zu einem einzigen Netzwerk verbinden (z.B. die LANs von auseinandergelegenen Fachbereichen)


## 88. Was ist Flusskontrolle und wie kann Flow Control gemacht werden?

Assume a fast sender and a slow receiver. If the sender doesn't throttle itself, the receiver's buffer is going to overflow and he will drop packets.

Solutions: stop-and-wait, sliding window with go-back-n, sliding window with selective repeat


## 89. Erklären Sie zwei kollissionsfreie Kanalzugriffsverfahren.

Token Ring:

* Empty information frames are continuously circulated on the ring.
* When a computer has a message to send, it seizes the token (the frame). The computer packs data into it and sends it on.
* The frame is then examined by each successive workstation. The workstation that identifies itself to be the destination for the message copies it from the frame and changes the token back to 0.
* When the frame gets back to the originator, it sees that the token has been changed to 0 and that the message has been copied and received. It removes the message from the frame.
* The frame continues to circulate as an "empty" frame, ready to be taken by a workstation when it has a message to send.

Token Bus:

A [token ring](token-ring) network except it's just a bus, where the ring is "simulated".

Sonst: CSMA/CD

The basic principle is sensing whether the wire is idle, and only sending packets when it is.

Collisions can still happen if two computers send at exactly the same time, so if that happens both computers will wait a random amount of time. If a client detects a collision, a jamming signal is sent to pause all communications and initiate the random wait.

CSMA/CD doesn't scale well because collisions are expensive, and the number of collisions rises as the number of clients rises. Particularly, if two machines become ready during a thrid machine's transmission, both will immediately send once that transmission ends. 


## 90. Wie kann ein Protokoll Fehlerfreiheit ohne Wiederholungen erreichen?

Nie garantiert, aber error correcting codes existieren, z.B. [Hamming encodings](#Hamming encodings) koennen n bit Fehler korrigieren, wenn die Hamming Distanz des Codes 2n+1 ist, aber auch nicht mehr als das - 

Of course, if you get errors that flip more than that, then you're SOL and you won't notice. You might even undertake a wrong "correction". But on the basis that such errors are less likely, doing corrections is worth it overall.


## 91. Wozu dient Redundanz in Leitungsprototkollen?

Fehler finden: parity bits, CRC

Fehler beheben: e.g. Hamming Codes

### Hamming Distance

The number of positions in which two bit words difffer:

```
01011001
01011010
-------- // XOR
00000011 // distance = 2
```

Equivalently, the number of `1`s if you XOR the words.


### Hamming encodings

If the minimum Hamming distance between any two words of your encoding is $n$, then you can: 

* detect any errors that flip at most $n-1$ bits. 
  * You cannot detect errors that flip more bits than that of course.
* *correct* errors that flip at most $\frac{n-1}{2}$ bits
  * because then the erroneous word is always going to be closer to one unique codeword than to all others

Of course, if you get errors that flip more than that, then you're SOL and you won't notice. You might even undertake a wrong "correction". But on the basis that such errors are less likely, doing corrections is worth it overall.



## 92. Wann ist FEC besser als ARQ?

FEC is for noisy channels - ARQ retransmission makes little sense if that's just as likely to have errors.

FEC makes little sense if the error rate is low.

If the transmission delay is massive, or the application is time sensitive, ARQ makes little sense.


## 93. Was ist ein RFC?

Ein RFC ist ein request for comments, kommt von der ISOC und ist dazu da, um neue Standards vorzuschlagen und zu diskutieren. RFCs sind eher informell und nicht alle werden von der IETF standardisiert.


## 94. Wie „entstehen“ Internet Standards?

Internet Standards entstehen aus einem oder mehreren RFCs und werden von der IETF ins Leben gerufen.


## 95. Was sind die wichtigsten Aufgaben der Netzwerkschicht?

Routing, handling congestion, moving data between networks with different specs (max packet size, protocols...)


## 96. Was bedeutet Robustheit eines Routing-Verfahrens?

* Overcoming changes in the [network topology](network-topology) since that will evolve a lot over the years



## 97. Was bedeutet Stabilität eines Routing-Verfahrens?

* The ability to quickly converge to a stable set of routes (unless the topology changes)



## 98. Was unterscheidet einen Host von einem Router?

Ein Host ist ein Endgeraet, z.B. ein Server oder einfach das Geraet eines Users.

Ein Router ist speziell dazu da, um Kommunikation ueber mehrere Netzwerke hinweg zu ermoeglichen.


## 99. Welche Vorteile/Nachteile hat Flooding von Routing-Informationen?

Not generally useful, except for broadcast addressing modes (for example, [wifi](wifi) is always a broadcast so that's flooding in fact). 

The upside is that it is *extremely* robust while also being simple, requiring no real setup, and always finding the fastest route (fulfilling some [routing](routing) design goals).

Can therefore be used to bootstrap other routing schemes.


## 100. Welchen Kriterien sollen Routing-Verfahren erfüllen?

Besides the obvious correctness, efficiency, simplicity:

* Robustness
  * Overcoming changes in the [network topology](network-topology) since that will evolve a lot over the years
* Stability
  * The ability to quickly converge to a stable set of routes (unless the topology changes)
* Handling congestion
* Providing some baseline quality of service



## 101. Skizzieren Sie den IPv4-Header und erklären Sie die Felder.

![IPv4 header](/Users/ariez/MEGA/Zettelkasten/iph.png)

* There's a field for the IP version of this packet so both ipv4 and [ip6](ip6) can be used
* Headers are variable-length up to 60 bytes so there's a header length field `IHL`
  * The length is given in the number of 32-bit words
* Total length field for the entire packet (called **maximum transmission unit** MTU: up to 64kb, in practice often 1500 bytes to fit inside one [ethernet](ethernet) frame)
* `Flags` and `Fragment offset`: related to [fragmentation](ip#Fragmentation)
* [TTL](packetization) counter which is a hop counter in practice
* `Protocol` field: what [transport layer](transport-layer) protocol this packet should be passed on to. This is a set of hardcoded identifiers maintained by a central authority
* Checksum 
* source/dest IPs
* Options: for experimental features and special usecases
  * example: The Security option tells how secret the information is. In theory, a military router might use this field to specify not to route packets through certain countries the military considers to be ‘‘bad guys.’’ In practice, all routers ignore it
  * Not really used in practice.



## 102. Was ist Broadcast Routing?

Das Problem der Verteilung einer Nachricht von einem Knoten an alle anderen Knoten in einem Netzwerk. Z.B. durch flooding erledigt.


## 103. Welche IPv4 Netzklassen gibt es?

Class A,B,C; wobei die network portion nur das erste, die ersten zwei oder die ersten drei Bytes sind.

Der Vorteil dieser hierarchiscxhen Aufteilung ist vereinfachtes Routing, weil routing decision nur auf Basis der network portion gemacht werden koennen. Der Nachteil ist wasteful management von Adressen, weil man nun irgendwie Netzwerke in Adressbloecke quetschen muss. In Class A Netzwerken sind beispielsweise wahrscheinlich viele Adressen wasted.


## 104. Was ist eine Subnetzmaske?

Die **netmask** ist die mittels logischem AND anzuwendende Bitmask, wenn man aus der ganzen IP nur die network portion will: für Class C z.B. `255.255.255.0`



## 105. Transparente/nicht transparente Fragmentierung

The basic difference is: do you reassemble the fragmented packet to the larger one once you passed through the low MTU portion of the network (transparent)? Or do you only reassemble at the destination (nontransparent)?

The first option is called transparent because the fact that the packet had to be chopped up is hidden from subsequent routers.



## 106. Mit welchem Verfahren kann man einen Datenstrom auf konstante Rate regeln und wann werden Pakete verworfen?




## 107. Mit welchem Verfahren kann man eine Datenstrom auf variable Rate regeln und wann werden Pakete verworfen?




## 108. Was ist ECN?


**Explicit Congestion Notification**. Instead of sending extra packets to notify of congestion (which would kind of worsen the whole deal), a `congestion` flag can be set in the [ip header](ip4#Header).

The recipient of the packet is then supposed to notify the sender of congestion. The sender should then throttle transmission. That, however, happens *above* the IP level, e.g. in [TCP](TCP) instead.

The problem of course is that there's some delay until the sender is actually notified of congestion and slows down. Whatever still is in the pipe will arrive at the congested router at full blast. Schemes to solve this are **Hop-by-hop backpressure** and **load shedding** (Tanenbaum p400f).



## 109. Was ist RED?

**Random Early Detection**: try to deal with congestion ASAP by starting load shedding (dropping packets) early. [TCP](TCP) will see packet loss as an indicator of congestion and is hardcoded to slow down when this occurs. So RED works by notifying the sender of congestion, just as ECN does, except it does so implicitly and early.



## 110. Welche Ansätze für Congestion Control kennen Sie?

(Over)provisioning, traffic-aware routing, ECN, RED, hop-by-hop backpressure, load shedding.



## 111. Was versteht man unter „over-provisioning“?

Mehr Ressourcen zu alloziieren als minimal notwendig waere, um etwas "Puffer" zu haben fuer bessere QoS


## 112. Welche Ansätze für Flow Control kennen Sie?

* Feedback-based flow control: wait for explicit feedback that you can send another packet, e.g. stop and wait or sliding window
* Rate-based flow control: agree to a certain maximum rate beforehand. That's typically done in the [transport layer](transport-layer)



## 113. Was kennzeichnet ein Open Loop Verfahren.




## 114. Erklären Sie das Token-Bucket Verfahren.

Leaky bucket: 

A way to smooth out traffic, put a cap on bandwidth but also allow bursts of higher data rates.

In general you have your leaky bucket where traffic flows in at a variable rate but out at a constant rate. Let the capacity be $B$ and the data rate $R$. Until the bucket is full, traffic can go arbitrarily fast; only once full will it have to be throttled to the draining rate. 

The **token bucket** is a slightly different analogy for the same thing: water flows in at a constant rate, and you drain it at a variable rate. The bucket holds your data "allowance" so to say.



## 115. Was ist Flusskontrolle und wie kann sie gemacht werden?

Flow control: fast sender, slow receiver. How do you limit the sender's data rate so it matches the capacity of the receiver?
 
* Feedback-based flow control: wait for explicit feedback that you can send another packet, e.g. stop and wait or sliding window
* Rate-based flow control: agree to a certain maximum rate beforehand. That's typically done in the [transport layer](transport-layer)


## 116. Nach welchen Kriterien kann ein kürzester Pfad in einem Netzwerk berechnet werden?

There are many things that should affect the edge weights: generally the [quality of connection](QoS) (bandwidth, delay, jitter, packet loss), but also current congestion



## 117. Was bedeutet Stabilität eines Routing-Verfahrens?

The ability to quickly converge to a stable set of routes (unless the topology changes)



## 118. Was unterscheidet Distance Vector von Link-state Routing?

Distance vector: nur lokale Sicht wird gebraucht, da wir Bellman-Ford Algorithmus benutzen

Link-state: die globale Netzwerktopologie wird an alle Router verteilt, damit wir Dijkstra benutzen koennen, welcher das count to infty Problem nicht hat


## 119. Was versteht man unter hierachischem Routing?

Networks are too large for every router globally to maintain an optimal route to every other router. Instead, divide the network into **regions**, then find optimal routes within regions only and delegate traffic into other regions to some gateway. You can also group the groups again, and again, as often as required.

Required is a mathematical term here as there's an actual [optimum](optimization) for the number of groupings: $\ln n$ (Kamoun and Kleinrock, 1979).




## 120. Was ist und wie kann man Jitter beeinflussen?

The variance in packet delay (ping). Particularly important in realtime communications (e.g. [WebRTC](WebRTC))

Ways to improve jitter:

* Add some artificial delay on the receiving end to buffer jitter away
* A queue where arriving packets wait for a bit so you can shuffle them back in order


## 121. Was unterscheidet tabellarisch Virtual-Circuit und Datagram Vermittlung.

* Connectionless: **datagram network**
  * To send packets, routers look at their routing table and choose a route *for each packet individually*
  * So packets which semantically belong together might get sent over different routes if we get notified of some [congestion](congestion) for example
* Connection-oriented: **virtual circuit**
  * Idea: try to avoid having to choose a new route every time
  * Establishing a connection just means choosing a route and remembering that for the rest of the communication
  * That route is called the virtual circuit. 
  * Each packet has an ID/label telling what VC it belongs to. No source/dest address is needed.
    * That ID may have to change between hops because it can't be globally unique (called **label switching**)
  * VCs are commonly used within ISP networks

Comparison table:

|                               | Datagram                                  | VC                                                       |
|-------------------------------|-------------------------------------------|----------------------------------------------------------|
| Verbindungsaufbau nötig       | Nö                                        | Joa                                                      |
| Adressinformation             | Sender + receiver                         | VC label                                                 |
| Zustandsinformation im Router | Nö                                        | Der VC muss gespeichert werden                           |
| Routing                       | Individuell                               | Vorher fixiert                                           |
| Routingfehler passiert        | Reroute                                   | Abbruch - VCs gehen verloren                             |
| Überlast                      | Packet loss ([load shedding](congestion)) |                                                          |
| [QoS guarantees](QoS)         | Schwierig: siehe [congestion](congestion) | Easy: preallocate resources upon establishing connection |



## 122. Was versteht man unter transparenter bzw nicht transparenter Fragmentierung?

The basic difference is: do you reassemble the fragmented packet to the larger one once you passed through the low MTU portion of the network (transparent)? Or do you only reassemble at the destination (nontransparent)?

The first option is called transparent because the fact that the packet had to be chopped up is hidden from subsequent routers.



## 123. Wie funktioniert Fragmentierung (am Beispiel IPv4)?

The necessary information for reassembly is held in the [IP Header](ip4#Header). There's a bit flag indicating whether the current fragment is the last one, and a field that indicates the *byte offset* of the fragment. An offset of $n$ says "this fragment comes $n$ bytes after the initial fragment". The reason we don't just do a seq id is so fragments that arrive out of order can already begin to be reassembled correctly in a buffer.



## 124. Was kann ein Router zur Berechnung von „besten“ Pfaden messen?

There are many things that should affect the edge weights: generally the [quality of connection](QoS) (bandwidth, delay, jitter, packet loss), but also current congestion



## 125. Wie und warum funktioniert Multicast-Routing anders als Unicast-Routing?

Multicast routing ist prinzipiell eher ein "targeted broadcast" als ein mehrfacher Unicast, weil:

If the group is small in terms of the whole network but large in absolute terms, neither a series of unicasts nor a broadcast is economical

The basic idea behind multicasts is therefore closer to a broadcast than a unicast: take the broadcast spanning tree and prune nodes that aren't in the recipients group and also don't have children that are


## 126. Nennen Sie die Kriterien eines Routing-Verfahrens

Besides the obvious correctness, efficiency, simplicity:

* Robustness
  * Overcoming changes in the [network topology](network-topology) since that will evolve a lot over the years
* Stability
  * The ability to quickly converge to a stable set of routes (unless the topology changes)
* Handling congestion



## 127. Was bedeutet Fairness eines Routing-Verfahrens?

* Some traffic shouldn't be prioritized over other traffic.



## 128. Was versteht man unter Flooding?

The basic idea: send every packet along every known path, except the one it came from.

This is not generally useful, except for [broadcast routing](broadcast) (for example, [wifi](wifi) is always a broadcast so that's flooding in fact). And the upside is that it is *extremely* robust while also being simple, requiring no real setup, and always finding the fastest route (fulfilling some [routing](routing) design goals).

Obviously this creates exponential load (even infinite load if there's a loop) so you will want to mitigate this:

* prevent infinitely traveling packets by adding a TTL field to the [packets](packetization)
* do not re-flood packets that have been flooded already 
  * how? remember each packet's seq number and source so you can identify packets you've already seen



## 129. Was kann ein Router bei Überlast tun?


### ECN

**Explicit Congestion Notification**. Instead of sending extra packets to notify of congestion (which would kind of worsen the whole deal), a `congestion` flag can be set in the [ip header](ip4#Header).

The recipient of the packet is then supposed to notify the sender of congestion. The sender should then throttle transmission. That, however, happens *above* the IP level, e.g. in [TCP](TCP) instead.

The problem of course is that there's some delay until the sender is actually notified of congestion and slows down. Whatever still is in the pipe will arrive at the congested router at full blast. Schemes to solve this are [#RED](#RED), **Hop-by-hop backpressure** (Tanenbaum p400f) and [#Load Shedding](#Load Shedding).


### Load Shedding

**Load Shedding** just means dropping packets.

The key question is just which ones? It depends on the application - for file transfers, old packets are worth more than new ones (dropping old packets while sending new ones just means the recipient has to buffer them and wait for the old ones); but for realtime communication new packets are probably more desirable. The former is called **wine** and the latter is called **milk**


### RED

**Random Early Detection**: try to deal with congestion ASAP by starting load shedding (dropping packets) early. [TCP](TCP) will see packet loss as an indicator of congestion and is hardcoded to slow down when this occurs. So RED works by notifying the sender of congestion, just as ECN does, except it does so implicitly and early.






## 130. Skizzieren Sie den IPv4 Header und erklären Sie die Felder?

![IPv4 header](/Users/ariez/MEGA/Zettelkasten/iph.png)

* There's a field for the IP version of this packet so both ipv4 and [ip6](ip6) can be used
* Headers are variable-length up to 60 bytes so there's a header length field `IHL`
  * The length is given in the number of 32-bit words
* Total length field for the entire packet 
* `Flags` and `Fragment offset`: related to [fragmentation](fragmentation)
* [TTL](packetization) counter which is a hop counter in practice
* `Protocol` field: what [transport layer](transport-layer) protocol this packet should be passed on to. This is a set of hardcoded identifiers maintained by a central authority
* Checksum 
* source/dest IPs
* Options: for experimental features and special usecases
  * example: The Security option tells how secret the information is. In theory, a military router might use this field to specify not to route packets through certain countries the military considers to be ‘‘bad guys.’’ In practice, all routers ignore it
  * Not really used in practice.




## 131. Welche IPv4-Adressen sind für Broadcast-, Private- und Link-Local-Adressen reserviert?

* the address 255.255.255.255 is reserved for a [broadcast](broadcast); any netmask followed by all 1's is reserved for a broadcast to that network.
* Reserved addresses for private local networks: 100.64.0.0/10, 172.16.0.0/12, 192.168.0.0/16
* Link-local addressses: 169.254.0.0/16
  * A link local address is an address valid only within the local network segment
  * What that segment is, exactly, depends on the type of the network; e.g. with [ethernet](ethernet) it's a single wire and all devices that are tapped into that.


## 132. Was ist CIDR, welche Vorteile bringt es und wie verwendet man es?

Es gab schlussendlich viel zu wenige (und viel zu riesige) Class A+B, und viel zu viele ungenutzte Class C Netzwerke. Man hat mit **CIDR** in 1993 dann auch "Zwischendinger" erlaubt, also $n$ Bits als network portion und nicht nur $n$ Bytes. Genannt wird das **variable-length subnet masking**.

Entsprechend gibt es nun auch netmasks der Form `255.255.255.252`; da das unwieldy/unreadable ist erlaubt man auch die kompaktere Schreibweise `192.168.8.1/30`, wobei die `/30` dafür steht, dass die Netmask 30 leading `1`s hat.

CIDR Subnet Blöcke werden von einer zentralen Authority an regionale Authorities verteilt, welche ihre Blöcke dann an ISPs weiterverteilen, usw.

* Vorteil:
  * Schnelleres Routing, weil routing decision nur auf Basis der network portion gemacht werden koennen:
    * Bsp. Ein [Router](router) in NY braucht keinen routing table entry für *jeden* Host mit der network portion `183.221`, wenn er weiß, dass diese network portion in Schottland liegt
    * es reicht, diese network portion zu einem schottischen Router zu bringen. 
      * Sollte es ein kleineres Subnet mit gleicher Prefix geben, z.B. `183.221.173`, und dieses liegt aber *nicht* in Schottland, so kann man einen weiteren routing table Entry für dieses Netzwerk anlegen. Beim Routing sucht man dann den Eintrag mit dem längsten Match.
  * Nicht-kaputte class sizes



## 133. Erklären Sie fünf ICMPv4 Kontrollnachrichten/Fehlermeldungen?

[IP](ip) Protocol for handling unexpected events that occur during [routing](routing).

Examples:

* Destination unreachable (no one has a route)
* Time exceeded ([TTL](packetization) hit 0)
* Redirect (there's a better route available)
* Echo, echo reply (check if machine is "alive" and reachable, used by ping)
* Router advertisement and solicitation (Advertise self as, or find an available router)


## 134. Wie funktioniert NAT anhand eines Beispiels?

Beispiel:

* 192.168.0.8 sendet Paket an 173.21.217.191
* NAT box faengt dieses Paket ab und schaut rein
* Sieht, dass das Paket an Port 3271 gesendet wurde
* Wuerfelt aus: Port 7232
* Mappt diesen Port an source port und ip
* Setzt die eigene IP und den neuen Port in das Paket ein, wuerfelt die Checksums neu
* Bei eingehenden Paketen mit Port 7232 wird in dem Mapping nachgeschaut und das Paket entsprechend weitergeleitet.


## 135. Skizzieren Sie die Funktionsweise von DHCPv4 anhand der Nachrichten.

Automatische Konfiguration von neuen Hosts in einem Netzwerk: welche IP, Netzmaske, Router, Timeserver...

Wenn ein Host gerade neu gestartet wird, hat er z.B. eine baked-in [ethernet](ethernet) [MAC](MAC) Adresse, aber noch keine IP. Mit DHCP holt er sich eine:

* Broadcaste eine request fuer eine IP-Adresse ans Netzwerk: `DHCP DISCOVER`
  * Damit wir eine Antwort kriegen koennen, enthaelt dieses Paket die MAC-Adresse.
* Der DHCP Server waehlt eine freie aus und antwortet mit einem `DHCP OFFER` Paket mit dieser Adresse.
  * Die Auswahl dieser freien Adresse geschieht auf 3 mögliche Wege:
    * Manuell: man bestimmt vorher ein fixes Mapping: MAC-Adresse $X$ bekommt IP $Y$; wer nicht im Mapping ist bekommt nix.
    * Automatisch: Adresse wird automatisch gewählt und auf unbestimmte Zeit ausgegeben.
    * Dynamisch: Auch automatisch gewählt, aber da Hosts sich nicht unbedingt abmelden und wir sonst ggf. ein Adressen-Leak haetten, sind dynamische Adressen nur fuer einen bestimmten Zeitraum valide (**leasing**). Hosts müssen vor Ablauf des Lease eine neue Adresse anfragen.



## 136. Welche Betriebsmodi bietet DHCPv4 und wofür werden sie verwendet?

* Manuell: man bestimmt vorher ein fixes Mapping: MAC-Adresse $X$ bekommt IP $Y$; wer nicht im Mapping ist bekommt nix.
* Automatisch: Adresse wird automatisch gewählt und auf unbestimmte Zeit ausgegeben.
* Dynamisch: Auch automatisch gewählt, aber da Hosts sich nicht unbedingt abmelden und wir sonst ggf. ein Adressen-Leak haetten, sind dynamische Adressen nur fuer einen bestimmten Zeitraum valide (**leasing**). Hosts müssen vor Ablauf des Lease eine neue Adresse anfragen.




## 137. Wie sehen die ARP-Nachrichten aus, damit Rechner A mit IPv4 Adresse IPA und MAC Adresse MACA die MAC Adresse MACB von Rechner B mit IPv4 Adresse IPB herausfindet? Skizzieren Sie den Aufbau eines ARP-Requests

Request:

FF:FF:FF:FF:FF:FF
X
EtherType
ARP header...
X
A
00:00:00:00:00:00
B

Response:

X
Y
EtherType
ARP header...
X
A
Y
B



## 138. Wie sieht ein ARP Paket aus?

| Byte   | Field | Meaning                                                               |   |   |
|--------|-------|-----------------------------------------------------------------------|---|---|
| 0-1    | HTYPE | [data link layer](data-link-layer) protocol type, e.g. 1 for ethernet |   |   |
| 2-3    | PTYPE | [network layer](network-layer) protocol type, e.g. ip4 is `0x0800`    |   |   |
| 4      | HLEN  | Data link layer address length                                        |   |   |
| 5      | PLEN  | Network layer address length                                          |   |   |
| 6-7    | OPER  | Operation type: 1 is request, 2 is reply                              |   |   |
| from 8 | SHA   | Sender hardware address (e.g. [MAC](MAC))                             |   |   |
|        | SPA   | Sender protocol address (e.g. ip4)                                    |   |   |
|        | THA   | Target hardware address (e.g. [MAC](MAC))                             |   |   |
|        | TPA   | Target protocol address (e.g. ip4)                                    |   |   |


So for example an ARP request in ip4 over ethernet might look like this:

```
0x0001
0x0800
0x06
0x04
0x0001
// Sender MAC...
// Sender IP...
// Target MAC...
// Target IP...
```



## 139. Was ist Gratuitous ARP, Proxy-ARP?

### Gratuitous ARP

To keep ARP caches up to date, have machines update their MAC/IP mappings whenever they catch an ARP request - you can take the sender's data.

Now, when a machine's state changes, have it broadcast an ARP request at itself. There won't be a reply, but others will see it and update their cache.


### Proxy ARP

ARP but the target is in another network and the router is specially set up to forward the request to that network.



## 140. Welches IPv4 Protokoll sendet welche Kontrollnachrichten/Fehlermeldungen?




## 141. An welches Routing-Protokoll ist BGP angelehnt und wie funktioniert es?

BGP is an inter-network routing algorithm that allows enforcement of various routing policies.

It's based on [distance vector](distance-vector) routing, but the distance is largely affected by policies, not [QoS](QoS), as that's hard to measure across networks which employ different technologies. In its routing lists, it also doesn't just maintain the next link to take, but also the whole path. So this is called **path vector**.

BGP routers connect to each other via [TCP](TCP) and then advertise transit to certain destinations, as per their policies. They sit at the edges of ISP networks, gateways to other ISP networks so to say.


### Example

![BGP route advertisement propagation](/Users/ariez/MEGA/Zettelkasten/bgp.png)

This image considers a route advertisement to prefix C. ([Why prefix?](ip4#Subnets)) Initially, the route begins in autonomous systen AS3 (e.g. an ISP network), and it says "to get to C, go over R3a in AS3."

AS2 receives this advertisement and sends it on to AS1, saying "to get to C, go to R2a in AS2, then to AS3."

Finally in AS1 the advertisement arrives and now AS1 knows a path to C: "via R1a, go to AS2, then AS3."




## 142. Welche Nachrichten nutzt OSPF wofür?

Open Shortest Path First inter-network routing protocol. Builds on [link state](link-state) (it abstracts the topology to a graph), but also handles [congestion](congestion).

It segregates the network into the following entities:

* ![ospf png](/Users/ariez/MEGA/Zettelkasten/ospf.png)
* The Autonomous System: the entire network
* Areas: Sub-networks.
* Backbone area: an area that all other areas are connected to
  * All traffic running between areas must go over the backbone (so OSPF is a [star topology](network-topology))
* Stub areas: Areas that are only connected to one other area
* Kinds of routers:
  * Routers may be multiple of these
  * Internal routers: Routers inside a single area.
    * From the outside, these are the only visible parts of areas
  * Area border routers: Routers that sit between two areas
  * Boundary routers: Routers that have a connection to the outside world


### Messages

To do the [link state](link-state) information exchange, OSPF defines a set of messages to be sent directly in [IP](ip) packets:


| Message              | Point                                 |
|----------------------|---------------------------------------|
| HELLO                | Find your neighbors                   |
| LINK STATE UPDATE    | Share your costs table                |
| LINK STATE ACK       | Confirm reception of LSU              |
| DATABASE DESCRIPTION | Share how current your set of LSUs is |
| LINK STATE REQUEST   | Request LSU from adjacent router      |




## 143. Wie funktioniert reverse-path forwarding?

To improve on the basic [flooding](flooding)-like broadcast algorithm:

* When a router receives a broadcast message from source $S$, check if it was received on the link that's noted as the fastest route to $S$
* If yes, broadcast it to all other neighbors
* If no, it's likely a duplicate broadcast message; discard it.



## 144. Was ist IP-Multicast? Welches Protokoll wird im LAN verwendet, welche im WAN?

Multicasting is useful for streaming a sports event to a ton of users concurrently, for example (see [routing algorithm](routing-algorithm))

In [ip4](ip4), each [class D address](ip4#Subnets) identifies a user group. There are >250M Class D addresses. Sending a packet to a class D address means a "best-effort attempt" will be made to deliver the packet to all users in that group.

Hosts manage a list of groups that they are interested in. Via a protocol called **IGMP**, they communicate these groups to a multicast router. The actual multicasting is then done by multicast [routing algorithms](routing-algorithm), generally via creating a spanning tree of the network and then pruning irrelevant parts:

* Creating the spanning tree uses [reverse path forwarding](reverse-path-forwarding) to improve [broadcast](broadcast) efficiency
* Within an [AS](routing-algorithm), an algorithm called **PIM** (protocol independent multicast) is used
* Between ASes, [BGP](BGP) has to be extended with multicast capabilities.



## 145. Welchen Zweck hat/Vorteil bietet VPN?

The idea behind a VPN is creating a private network, say between company offices, on top of [ip](ip). You'd create an encrypted connection between offices and firewall off all other traffic.

The advantage of private networks is that no one can listen into the traffic without physically wiretapping, because in a private network you have your own infrastructure. Doing this virtually means using the internet to emulate private connections and encrypting the traffic; the advantage being you don't actually need to lay down your own lines.

With commercial VPN services the obvious problem is that while no one may well be able to look at the actual contents of the traffic, there's an attack vector in the service provider.



## 146. Was sind die Unterschiede zw Leitungs-/Nachrichten- und Paketvermittlung?




## 147. Nennen Sie 2 Tunneling-Protokolle und ihre Funktionsweise?




## 148. Wie funktioniert ein VPN?




## 149. Warum findet IPv4 noch Verwendung, obwohl es IPv6 schon länger gibt?

* Weil Unmengen an alter Infrastruktur umgestellt werden muss
* Weil es NAT gibt und wir scheinbar irgendwie doch noch eine Weile mit IPv4 auskommen
* Weil es keine backwards compatibility gibt, darum muessen Hosts beides in parallel betreiben und IPv4 stirbt noch langsamer


## 150. Was characterisiert das „Tactile Internet“? Geben Sie Anwendungsbeispiele dafür.




## 151. Was sind die wichtigsten Aufgaben/Funktionen der Netzwerkschicht?

Get packets from the source to the destination through the network. This is the first [layer](OSI) to not operate between two machines only, and it's largely done by ISPs and your local router.

* [routing](routing)
  * Includes handling congestion
* Handling networks with differing specs such as MTU or different protocols and so on.
  * [fragmentation](fragmentation)
* Meeting a certain [QoS](QoS)



## 152. Skizzieren und erklären sie Tunneling Mechanismen für IPv6/IPv4.




## 153. Skizzieren Sie den IPv6 Header und erklären Sie die Bedeutung der Felder.

The basic header is much simpler than the [ip4](ip4) one, but there can be multiple extension headers. What's gone is [fragmentation](fragmentation) stuff because we're now using path MTU discovery, and the checksum because it's slow and networks are now reliable enough and other layers have checksums too.

![The header](/Users/ariez/MEGA/Zettelkasten/ip6h.png)

| Bits    | Field                   | Description                                                              |
|---------|-------------------------|--------------------------------------------------------------------------|
| 0-3     | Version                 | IP protocol version: hardcoded 6                                         |
| 4-11    | Differentiated services | Congestion signaling, specifying desired [QoS](QoS)                      |
| 12-31   | Flow label              | Like an ID for related packets so a pseudo-connection can be established |
| 32-47   | Payload length          |                                                                          |
| 48-55   | Next header             | Tells which extension headers follow; if none, specifies TCP/UDP         |
| 56-63   | Hop limit               | TTL                                                                      |
| 64-195  | Source address          |                                                                          |
| 196-319 | Destination address     |                                                                          |





## 154. Charakterisieren Sie IPv6?

A replacement protocol for ip4 that mainly needed to solve the lack of addresses, but also addressed performance issues (checksum, routing table size, fragmentation), and improved/added features (better multicast, future proofing)



## 155. Wie funktioniert NDP?

NDP is the IPv6 replacement for [ARP](ARP). It defines new [ICMP](ICMP) packet types and improves on e.g. **neighbor unreachability detection**, the ability to detect when a neighbor is no longer reachable.

| Packet type            | Function                                                                                               |
|------------------------|--------------------------------------------------------------------------------------------------------|
| Router Solicitation    | Solicit a router to send out advertisement                                                             |
| Router advertisement   | Router advertises its existence and information such as global [address prefix](ip6#Autoconfiguration) |
| Neighbor solicitation  | Basically, ping a certain neighbor                                                                     |
| Neighbor advertisement | Positive ping reply                                                                                    |
| Redirect               | For a router to notify a host of a better route                                                        |



## 156. Wie funktioniert die IPv6 Autokonfiguration?

1. Generate a link-local address (64bit hardcoded prefix + 48 bit [MAC](MAC) address + filler)
2. **Duplicate Address Detection**: Send a [NDP](NDP) neighbor solicitation message targeted at that address, if nothing comes back we can use that address
3. Send a router solicitation and wait for a router advertisement
4. The router should answer with a globally unique 64bit prefix from which we can derive a globally unique ipv6 address.



## 157. Beschreiben Sie die fünf Nachrichten, mit denen IPv6 autokonfiguriert wird.

| Packet type            | Function                                                                                               |
|------------------------|--------------------------------------------------------------------------------------------------------|
| Router Solicitation    | Solicit a router to send out advertisement                                                             |
| Router advertisement   | Router advertises its existence and information such as global [address prefix](ip6#Autoconfiguration) |
| Neighbor solicitation  | Basically, ping a certain neighbor                                                                     |
| Neighbor advertisement | Positive ping reply                                                                                    |
| Redirect               | For a router to notify a host of a better route                                                        |



## 158. Wie funktioniert die Vermeidung von Adresskonflikten bei IPv6?

Es gibt eine 64-bit globally unique address prefix fuer Router, die von ISPs zugeteilt wird. Damit verbundene Hosts haben immer noch eine 48bit MAC; aus der MAC, der Prefix und 16 Filler-Bits setzt sich eine globally unique device address zusammen.


## 159. Erklären Sie die Aufgaben von ICMPv6.

Prinzipiell gleich wie bei ip4: [IP](ip) Protocol for handling unexpected events that occur during [routing](routing).

The IP6 version does more in that it's not just about error messages, but it also defines packet types for messages that were previously handled ARP, e.g. NDP, or for messages pertaining to roaming mobile hosts.


## 160. Diskutieren Sie den Einsatz von NAT6?

* Doing NAT between ip4 and ip6 nets us some interoperability
  * but as with NAT in general it only really makes sense when the host behind the NAT initiates contact
* IPv6 traffic is routed through some gateway which maintains a mapping from IPv6 to IPv4 address, again replacing the IPv6 address with a mapped IPv4 address
* However the gateway still needs an IPv4 address.
* It's basically a way to use IPv6 addresses in your LAN while the rest of the internet is still using IPv4 and therefore of limited use.



## 161. Erklären Sie die Herausforderungen beim Umstieg auf IPv6.

* Weil Unmengen an alter Infrastruktur umgestellt werden muss
* Weil es NAT gibt und wir scheinbar irgendwie doch noch eine Weile mit IPv4 auskommen
* Weil es keine backwards compatibility gibt, darum muessen Hosts beides in parallel betreiben und IPv4 stirbt noch langsamer



## 162. Was ist IPv4/IPv6 Translation und was ist dabei zu beachten?

* Doing NAT between ip4 and ip6 nets us some interoperability
  * but as with NAT in general it only really makes sense when the host behind the NAT initiates contact
* IPv6 traffic is routed through some gateway which maintains a mapping from IPv6 to IPv4 address, again replacing the IPv6 address with a mapped IPv4 address
* However the gateway still needs an IPv4 address.
* It's basically a way to use IPv6 addresses in your LAN while the rest of the internet is still using IPv4 and therefore of limited use.



## 163. Beschreiben Sie die Transition zu IPv6 mit NAT64.




## 164. Diskutieren Sie den Einsatz von „Happy Eyeballs“.


