# 14-05-2019

<!--TOC-->

## 

Zur Umsetzung der Automation wird ein geometrisches Modell verwendet, welches sog. "Tracks" in einem Raster modelliert. Tracks bestehen aus Segmenten unterschiedlicher Laenge und "Curvature". Segmente werden aneinandergeklebt, wobei die Orientation natuelich auch beruecksichtigt wird. Dies bildet ein mathematisches Modell, womit man dann aus einer informatischen Perspektive gut arbeiten kann.

Auf dieses Modell aufbauend kann man dann einen Graphen konstruieren, mit dem Algorithmen fuer Routing etc. implementiert werden koennen. Zum Beispiel muessen Schranken fuer die Beschleunigung von Shuttles implementiert werden, um die Mechanik zu schonen. Eine Frage waere dann, was die schnellste Route von A nach B waere, die eine bestimmte Schranke fuer die Beschleunigung nicht ueberschreitet. Dies in Echtzeit zu implementieren ist ein seit 25 Jahren offenes Problem! Existierende Algorithmen liefern nicht die optimale Loesung.

Shuttles koennen als Skalare auf Sektoren (als Pfad modelliert) werden: z.B. ist Shuttle-Position 3 auf einem Sektor die Position, die erreicht wird, wenn man den Pfad des Sektoren fuer 3 Laengeneinheiten folgt. Mit diesem System bewegt man sich nicht mehr einfach in R^3, sondern in einem komplexeren Modell. Shuttles koennen in diesem Modell auch von mehreren Segmenten auf einmal aktuiert werden.

Ein weiteres Thema ist Kollisionsvermeidung. Der Kunde hat die Voraussetzungen, um Kollisionen von Shuttles zu vermeiden, nicht; also miss sich B&R darum kuemmern. Shuttles koennten in unterschiedlichen Geschwindigkeiten Trajektorien folgen, ein Problem ist also die Update-Frequenz: wenn man sie anhand der Zeit fixiert, bekommt man ungenauere Positions-Updates bei hohen Geschwindigkeiten. Weiters sollen lokale Geschwindigkeitsbegrenzungen implementiert werden, um zu vermeiden, dass sich Shuttles in Kurven einfach verabschieden. Diese Begrenzungen muessen zusaetzlich zu den Limits fuer die Beschleunigung aktiv sein.

Weiters ist eine Frage, wie das Zuruecklegen grosser Distanzen modelliert werden soll. Sektoren sollen allgemein nur lokal sein. Deadlocks koennen hier auch auftreten: ein Shuttle kann auf einen anderen Shuttle warten, der auf ihn selbst wartet. Um mit Deadlocks umzugehen, hat man mehrere Optionen. Die naheliegendste Loesung waere Erkennung und Aufloesung von Deadlocks. Dies ist aber in diesem Modell schwierig, da sich Akteure zurueckbewegen muessen, was ganz neue Probleme erzeugen kann. Sinnvoller waere es, beweisbar deadlock-freie Logistikalgorithmen zu entwerfen.

Schlussendlich betrachten wir von einer Business-Seite aus Forschung und R&D in der Industrie. In Academia gibt es keine Patente etc, in der Industrie ist das gesetzte Ziel das Verbessern des eigenen Produktes. Daher werden Patente eingereicht statt Papers veroeffentlicht. Dennoch wird viel mit Universitaeten kooperiert. Weiters liegt die Forschung in der Industrie oft in der Schnittmenge verschiedener Bereiche. 
