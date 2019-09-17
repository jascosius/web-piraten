# Aufgaben

Hier findest du die aktuellen Aufgaben des Schnupperstudiums.
Speichert die Lösungen zur Aufgabe *n* als "*n*.py"
und gegebenenfalls mit einer weiteren Zahl für die Unteraufgabe.
Die 7. Aufgabe würde also zum Beispiel unter "07.py" abgespeichert und Aufgabe 1, Teil 1, als "01-1.py". Es ist auch eine gute Idee, zu jeder Aufgabe eine Beispielwelt zu speichern.

Da die Aufgaben aufeinander aufbauen, sollten sie der Reihe nach bearbeitet werden. In den Aufgaben zu einer Vorlesung habt ihr immer nur die Sprachkonzepte zur Verfügung, die in den bisherigen Vorlesungen vorgestellt wurden!


## Vorlesung 1: Grundlagen

Für die Lösung dieser Aufgaben habt ihr lediglich Aktionen, simple Funktionen sowie Verzweigungen zur Verfügung, aber noch keine Schleifen oder Rekursion!


### Aufgabe 1

Schreibt ein Piraten-Programm, welches die Piraten in einem vollen Schatzfeld acht Schätze aufnehmen lässt. Geht davon aus, dass das Piratenschiff sich zu Beginn des Programms in der linken oberen Ecke des Feldes befindet und nach rechts guckt.

1. In dieser Teilaufgabe sollen die Schätze in waagerechter Richtung aufgenommen werden.

1. In dieser Teilaufgabe sollen die Schätze in diagonaler Richtung aufgenommen werden.


### Aufgabe 2

Schreibt Programme, die die Piraten auf einem vollen Schatzfeld folgende Bilder erzeugen lassen:

1. <img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge2a.jpg">

1. <img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge2b.jpg">

Falls euer Schatzfeld größer ist als das in den Abbildungen könnt ihr euch dort auf die linken 10 mal 10 Felder beschränken.


### Aufgabe 3

Betrachtet noch einmal Aufgabe 2.1 und schreibt ein Programm, welches jenes Bild auf einem _leeren_ Schatzfeld erzeugt. Könnt ihr euch eine Funktion so überlegen, dass ihr sie für alle vier Schatz-Dreiecke benutzen könnt?

Euer Programm muss wieder nur auf den linken 10 mal 10 Feldern funktionieren, nicht auf einem Schatzfeld beliebiger Größe. Auch kann euer Piratenschiff keine Schätze ablegen, sondern nur Bojen. Das von euch erzeugte Bild wird in dem Punkt also vom Bild in Aufgabe 2.1 abweichen.


### Aufgabe 4

Lasst euer Piratenschiff eine Spur von 128 Bojen produzieren. Wie könnt ihr das mit möglichst wenig Code erreichen? Denkt dran, dass ihr noch keine Schleifen benutzen dürft!

Da das Schatzfeld deutlich kleiner ist könnt ihr euer Programm testen, indem ihr euch auf eine Spur von 8 Bojen beschränkt.



## Vorlesung 2: Rekursion und Schleifen

Im Folgenden verwenden wir zwar einzelne Szenarien zur Motivation der Aufgaben, eure Programme sollen aber auch auf ähnlichen Szenarien arbeiten. Die wirklichen Einschränkungen an die Szenarien findet Ihr jeweils in der Aufgabenstellung.

Ab sofort stehen euch für das Lösen der Aufgaben auch Rekursion und Schleifen zur Verfügung, aber noch keine Variablen!


### Aufgabe 5

Schreibt ein Programm, mit dem die Piraten jeweils eine Boje unter alle Schätze einer Zeile setzen. Neben Schätzen können sich in der Zeile auch freie Felder, Monsterwellen und Kraken befinden. Geht davon aus, dass die Piraten im linken Feld der zu kopierenden Zeile stehen und dieses Feld frei ist. Das gewünschte Programmverhalten sollte an folgendem vorher/nachher Beispiel klar werden:

Vorher:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge5a.jpg">

Nachher:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge5b.jpg">

Wo sich die Piraten am Programmende befinden ist egal.

1. Löst die Aufgabe nur mit Rekursion, ohne Schleifen zu benutzen.

1. Löst die Aufgabe jetzt nur mit Schleifen, ohne Rekursion zu benutzen.


### Aufgabe 6

Die Piraten sollen eine Schatzspur aufnehmen. Sie sollen stehen bleiben, wenn sie keine Schatz mehr sehen.

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge6.jpg">

Geht davon aus, dass die Piraten am Anfang der Schatzspur stehen und jeder Schatz in der Spur neben maximal zwei anderen Schätzen liegt. Nehmt euch aber vor Monsterwellen und Kraken in Acht. Löst diese Aufgabe mit Hilfe von Rekursion, nicht mit Schleifen!


### Aufgabe 7

Verändert euer Programm zu Aufgabe 6 so, dass die Piraten nach dem Aufnehmen der kompletten Schatzspur jene wieder in Form von Bojen ablegen.


### Aufgabe 8

Schön wäre es natürlich, wenn die Piraten nicht nur eine Spur aufnehmen könnten, sondern beim Zurücklaufen auch weitere abzweigende Spuren aufnehmen könnten (Piraten sind ja doch sehr gierig). Überlegt, wie ihr euer Programm aus Aufgabe 6 oder 7 verändern könnt, damit die Piraten auch auf ihrem Rückweg suchen. Dabei ist es uns egal, ob die Schatzspur danach mit Bojen markiert ist oder nicht.

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge8.jpg">


### Aufgabe 9 (Zusatzaufgabe)

Die Piraten sollen aus einem durch Monsterwellen begrenzten Labyrinth herausfinden. Der Ausgang ist mit einem Schatz markiert.

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge9.jpg">

Überlegt, ob euer Algorithmus immer den Ausweg aus einem Labyrinth finden kann. Konstruiert Situationen, in denen euer Programm fehlschlägt. Versucht, euer Programm zu verbessern.



## Vorlesung 3: Parameter und Variablen

In diesen Aufgaben könnt Ihr davon ausgehen, dass die Felder keine Wellen oder Kraken enthalten. Außerdem könnt Ihr davon ausgehen, dass gegebenenfalls bestimmte Randzeilen und -spalten keine Schätze enthalten. Solltet Ihr später noch Zeit haben, könnt Ihr Eure Lösungen für Spezialfälle zusätzlich anpassen.

Ab jetzt habt ihr auch Funktionsparameter und Variablen zur Verfügung, aber noch keine komplen Datenstrukturen!


### Aufgabe 10

Die Piraten sollen in einem leeren Feld ein Quadrat mit einer vorgegebenen Kantenlänge mit Bojen markieren. Achtet darauf, dass alle möglichen Kantenlängen funktionieren.

1. Malt zunächst nur ein Quadrat mit fest von euch vorgegebener Seitenlänge.

1. Lasst die Piraten nun zuerst Schätze vor ihnen einsammeln und deren Anzahl als Kantenlänge für das Quadrat verwenden.


### Aufgabe 11

Die Piraten sollen überprüfen, ob zwei aufeinander folgende Zeilen gleich viele Schätze enthalten. Ist dies der Fall, so sollen sie in die erste Spalte der zweiten Zeile laufen, ansonsten in die letzte Spalte der zweiten Zeile.


### Aufgabe 12

Die Piraten haben ihre Welt mal wieder dezent unaufgeräumt hinterlassen. Schreibt ein Programm, welches zunächst alle Schätze einsammelt um sie dann, beginnend in der unteren linken Ecke, aufeinanderfolgend wieder auszulegen. Sollte für die Ausgabe eine Zeile nicht ausreichen, könnt Ihr mehrere Zeilen verwenden.

Geht davon aus, dass die Piraten am Anfang in der oberen linken Ecke stehen und nach rechts schauen.


### Aufgabe 13

Schreibt ein Programm, mit dessen Hilfe die Piraten herausfinden können, in welcher  Zeile sich die meisten Schätze auf dem Feld befinden. In dieser Zeile sollen die Piraten nach Ablauf des Programms stehen bleiben. Falls die Zeile mit den meisten Schätzen nicht eindeutig ist, können die Piraten in einer beliebigen Zeile mit den meisten Schätzen stehen bleiben.

Vernachlässigt zunächst die Schätze in der ersten Zeile (nicht zählen!). Wer will kann sie später noch in den Algorithmus aufnehmen.


### Aufgabe 14 (Zusatzaufgabe)

Implementiert in Aufgabe 9.2 sowohl das Zählen der Schätze als auch das Zeichnen des Quadrates ohne Schleifen zu verwenden, also nur mit Rekursion und Parameterübergabe.


### Aufgabe 15 (Zusatzaufgabe)

Ändert Aufgabe 9 so ab, dass das Quadrat um 45° verdreht dargestellt wird, also auf einer Spitze stehend.


### Aufgabe 16 (Zusatzaufgabe)

Bei dieser Aufgabe stehen die Piraten in der linken oberen Ecke des Feldes. Vor ihnen befinden sich Schätze und leere Felder, welche die binären Ziffern 1 und 0 repräsentieren. Diese binären Ziffern ergeben eine Binärzahl, welche zur Erkennung des Zahlenendes mit einer Welle abschließt. Die Piraten sollen diese Binärzahl (z.B. 11001) einlesen und in dem entsprechden Feld (hier z.B. das 25. Feld) eine Markierung ablegen. Um das Feld möglichst effizient zu finden, könnt ihr von einer festen Feldbreite ausgehen. Wer möchte, kann das Programm noch so erweitern, dass es mit beliebigen Feldgrößen selbständig klarkommt.


### Aufgabe 17 (Zusatzaufgabe)

Schreibt ein Programm, welches die Piraten zunächst vor ihnen liegende Schätze einsammeln lässt. Die Anzahl der Schätze bestimmt den Radius eines nun mit Bojen zu "zeichnenden" Kreises, der seinen Mittelpunkt in einem Feld der Welt haben soll. Wir gehen davon aus, dass die Seitenlänge eines Feldes 1 ist. Ein Feld soll nun mit einer Boje markiert werden, wenn der Abstand seines Mittelpunkts zum Mittelpunkt des zentralen Feldes kleiner oder gleich dem Radius des zu zeichnenden Kreises ist.

1. Geht zunächst von einer festen Anzahl an Feldern in der Piratenweld aus.

1. Erweitert euer Programm nun so, dass es mit beliebigen Feldgrößen klarkommt.



## Vorlesung 4: Datenstrukturen


### Aufgabe 18

Zwei Zeilen können ein Spiegelbild voneinander sein. Das gilt zum Beispiel für die folgenden beiden Zeilen:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge18.jpg">

Die Piraten sollen überprüfen, ob zwei aufeinanderfolgende Zeilen jeweils das Spiegelbild voneinander sind. Benutzt zur Lösung der Aufgaben Listen.


### Aufgabe 19

Die Piraten sollen sich eine Zeile (bestehend aus Schätzen und freien Feldern) merken und eine andere Zeile suchen, die mit der ersten Zeile identisch ist. In dieser Zeile sollen sie stehen bleiben. Ist keine Zeile mit der ersten Zeile identisch, sollen sie zurück in die erste Zeile laufen.


### Aufgabe 20

Schreibt ein Programm, welches sich ähnlich wie das Programm aus Aufgabe 7 verhält. Allerdings sollen die Bojen nicht in umgekehrter Reihenfolge gelegt werden. Vielmehr sollen die Piraten an den Ausgangspunkt zurücksegeln und von dort die zunächst aufgesammelte Spur nachträglich mit Bojen markieren und am Ende der Spur stehen bleiben. Könnt Ihr die aufgesammelten Informationen auch dazu verwenden, zum Ausgangspunkt zurückzukehren, also hierzu nicht die Rekursion verwenden zu müssen?


### Aufgabe 21

Schreibt ein Programm, welches sich wie das Programm aus Aufgabe 8 verhält, dazu allerdings nicht Rekursion, sondern Listen verwendet. Was würdet ihr in die Listen stecken?


### Aufgabe 22 (Zusatzaufgabe)

Die Piraten sollen durch einen Spurcode gesteuert werden können. Der Spurcode ist eine Folge von Schätzen, Bojen und freien Feldern, welche durch eine Monsterwelle abgeschlossen wird. Damit der Spurcode auch mehr als 10 Zeichen lang sein kann, darf er auch mehrere Zeilen lang sein. Im Spurcode steht jedes freie Feld für einen Schritt nach vorne, jeder Schatz für eine Drehung nach links und jede Boje für eine Drehung nach rechts.

Schreibt ein Programm, welches die Piraten zunächst einen Spurcode (welcher direkt vor den Piraten beginnen sollte) einlesen lässt und dann die Spurfolge interpretiert, die Piraten also von der linken oberen Ecke aus an eine beliebige Feldposition navigieren lässt und dort abschließend eine Boje ablegt.


### Aufgabe 23 (Zusatzaufgabe)

Aufgabe 18 kann auch ohne Parameter und Datenstrukturen gelöst werden. Die Idee ist wieder die Verwendung von Rekursion.
