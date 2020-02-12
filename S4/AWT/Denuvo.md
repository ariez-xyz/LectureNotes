# 07-05-2019

<!--TOC-->

## 

Es gibt einen Unterschied zwischen statischer und dynamischer binary analysis - statische Analyse faengt z.B. sich selbst modifizierenden Code nicht. Ein weiteres Problem besteht darin, Code von Daten zu unterscheiden. Hier wird ein Profiler eingesetzt: das Spiel wird gespielt und waehrenddessen analysiert in Hinsicht auf gut/weniger gut schuetzbare Funktionen. 

Die Implementation von dem Schutz wird dann umgesetzt, indem geeignete Funktionen zu Calls an einen Server ersetzt werden. Dem Server wird ein Funktionscode und eine Lizenz gegeben, und er soll dann einen der urspruenglichen Instruktion aequivalenten Wert zurueckgeben. Die Lizenz ist direkt an die Hardware gebunden, deshalb wuerde das Spiel auf einem anderen Computer nicht mehr funktionieren. 

Nach diesem Prozess wird das Spiel noch auf Performance optimiert, weil der Profiler nicht immer perfekt arbeitet. 

## 

Warum baut Denuvo Kopierschutz? Das Ziel ist, die ersten paar Wochen nach Release des Spiels zu schuetzen. Dort passiert der meiste Umsatz fuer den Publisher. Der Kopierschutz wird auf das Binary angewendet, weil dies den Workflow des Publishers erleichtert, auch wenn es eigentlich einfacher waere, den Quellcode zu veraendern. Ein weiteres Ziel ist, die Aenderungen an der Binary so vorzunehmen, dass sie moeglichst schwer zu reverse engineeren sind. Insbesondere gilt dies fuer den Lizenzcheck, dieser soll besonders schwer zu finden und auszubauen sein. 

Wie wird also die Lizenzfunktion versteckt? Mit binary obfuscation. Der Code soll schwer lesbar gemacht werden, was auch Aenderungen an dem Code erschwert. Dabei muss natuerlich das Originalverhalten des Programms beibehalten werden und die Laufzeit und Groesse des Programms sind beschraenkt. 

Wir betrachten die Limits von code obfuscation in der Theorie: Per Definition von Security mit dem Begriff von virtual black box security (d.h. nur Untersuchung vom In/Output zu einem Programm und Analyse dessen Verhaltens ist erlaubt) ist Obfuscation unmoeglich zu knacken. In der Theorie ist dies mit polynomiellem Laufzeitzuwachs moeglich, was aber fuer die Praxis viel zu schlecht ist. Daher sucht man nach anderen Moeglichkeiten wie z.B. einer konstruierten VM, die den Code ausfuehren soll und deren Instruction Set Architecture verschleiert/unbekannt ist. Ein Beispiel fuer eine verschleierte Instruktion waere ein "opaque predicate": ein boolescher Ausdruck, der immer wahr oder falsch ist, aber auf eine schwer nachvollziehbare Art und Weise. Bei Untersuchung eines Programmes muesste dann sowohl die wahre als auch falsche Variante beruecksichtigt werden, obwohl nur eine immer wahr waere. 

## Anti-Cheat

Denuvo bietet auch Massnahmen gegen Cheating wie das Hacken von ingame-currency, Wallhacks, Aimbots etc. fuer bestimmte Genres an, weil Cheating die Erfahrung fuer "normale" Spieler zerstoert.
