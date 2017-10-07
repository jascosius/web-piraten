# Aufgaben

Hier findest du die aktuellen Aufgaben des Schnupperstudiums.
Speichert die Lösungen zur Aufgabe *n* als "*n*.py"
und gegebenenfalls mit einer weiteren Zahl für die Unteraufgabe.
Die 7. Aufgabe würde also zum Beispiel unter "07.py" abgespeichert und Aufgabe 1 Teil 1 als "01-1.py".



### Liste aller Aufgaben

**Da die Aufgaben aufeinander aufbauen, sollten sie der Reihe nach bearbeitet werden.**

 [Aufgabe 1](#auf1 "Aufgabe 1")  &emsp; &emsp; &emsp;  [Aufgabe 6](#auf6 "Aufgabe 6")     &emsp; &emsp; &emsp;  [Aufgabe 11](#auf11 "Aufgabe 11") &emsp; &emsp; &emsp;  [Aufgabe 16](#auf16 "Aufgabe 16")<br>
 [Aufgabe 2](#auf2 "Aufgabe 2")  &emsp; &emsp; &emsp;  [Aufgabe 7](#auf7 "Aufgabe 7")     &emsp; &emsp; &emsp;  [Aufgabe 12](#auf12 "Aufgabe 12") &emsp; &emsp; &emsp;  [Aufgabe 17](#auf17 "Aufgabe 17")<br>
 [Aufgabe 3](#auf3 "Aufgabe 3")  &emsp; &emsp; &emsp;  [Aufgabe 8](#auf8 "Aufgabe 8")     &emsp; &emsp; &emsp;  [Aufgabe 13](#auf13 "Aufgabe 13") &emsp; &emsp; &emsp;  [Aufgabe 18](#auf18 "Aufgabe 18")<br>
 [Aufgabe 4](#auf4 "Aufgabe 4")  &emsp; &emsp; &emsp;  [Aufgabe 9](#auf9 "Aufgabe 9")     &emsp; &emsp; &emsp;  [Aufgabe 14](#auf14 "Aufgabe 14") &emsp; &emsp; &emsp;  [Aufgabe 19](#auf19 "Aufgabe 19")<br>
 [Aufgabe 5](#auf5 "Aufgabe 5")  &emsp; &emsp; &emsp;  [Aufgabe 10](#auf10 "Aufgabe 10")  &emsp; &emsp; &emsp;  [Aufgabe 15](#auf15 "Aufgabe 15") &emsp; &emsp; &emsp;  [Aufgabe 20](#auf20 "Aufgabe 20")



## Grundlagen

<a name="auf1"></a>

### Aufgabe 1

Schreibt ein Piraten-Programm, welches die Piraten in einem vollen Schatzfeld acht Schätze aufnehmen lässt. Geht davon aus, dass das Piratenschiff sich zu Beginn des Programms in der linken oberen Ecke des Feldes befindet und nach rechts guckt.

Die Schätze sollen in folgender Richtung aufgenommen werden:

1. Waagerecht

1. Diagonal



<a name="auf2"></a>

### Aufgabe 2

Schreibt ein Programm, das in einem vollen Feld alle Schätze vor den Piraten bis zum Rand aufsammelt. Am Rand sollen die Piraten stehen bleiben.



<a name="auf3"></a>

### Aufgabe 3

Schreibt Programme, die die Piraten auf einem vollen Schatzfeld folgende Bilder erzeugen lassen:

1. <img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge3a.jpg">

1. <img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge3b.jpg">

Funktionieren Eure Programme auch auf Feldern beliebiger Größe? Welche Einschränkungen gibt es?



<a name="auf4"></a>

### Aufgabe 4 (Zusatzaufgabe)

Betrachtet noch einmal Aufgabe 3.1 und schreibt ein Programm, welches jenes Bild auf einem _leeren_ Schatzfeld erzeugt. Könnt ihr euch eine Funktion so überlegen, dass ihr sie für alle vier Schatz-Dreiecke benutzen könnt?

Euer Programm braucht nicht auf beliebig großen Schatzfeldern zu funktionieren.



## Rekursion

**Im Folgenden verwenden wir zwar einzelne Szenarien zur Motivation der Aufgaben, eure Programme sollen aber auch auf ähnlichen Szenarien arbeiten. Die wirklichen Einschränkungen an die Szenarien findet Ihr jeweils in der Aufgabenstellung.**



<a name="auf5"></a>

### Aufgabe 5

Schreibt ein Programm, mit dem die Piraten jeweils eine Boje unter alle Schätze einer Zeile setzen. Neben Schätzen können sich in der Zeile auch freie Felder, Monsterwellen und Kraken befinden. Geht davon aus, dass die Piraten in der ersten Spalte der zu kopierenden Zeile stehen und dieses Feld frei ist. Das gewünschte Programmverhalten sollte an folgendem vorher/nachher Beispiel klar werden:

Vorher:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge4a.jpg">

Nachher:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge4b.jpg">

Wo sich die Piraten am Programmende befinden ist egal.

1. Löst die Aufgabe nur mit Rekursion, ohne Schleifen zu benutzen.

1. Löst die Aufgabe jetzt nur mit Schleifen, ohne Rekursion zu benutzen.



<a name="auf6"></a>

### Aufgabe 6

Die Piraten sollen eine Schatzspur aufnehmen. Sie sollen stehen bleiben, wenn sie keine Schatz mehr sehen.

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge5.jpg">

Geht davon aus, dass die Piraten am Anfang der Schatzspur stehen und jeder Schatz in der Spur neben maximal zwei anderen Schätzen liegt. Nehmt euch aber vor Monsterwellen und Kraken in acht.



<a name="auf7"></a>

### Aufgabe 7

Die Piraten sollen aus einem durch Monsterwellen begrenzten Labyrinth herausfinden. Der Ausgang ist mit einem Schatz markiert.

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge6.jpg">

Überlegt, ob euer Algorithmus immer den Ausweg aus einem Labyrinth finden kann. Konstruiert Situationen, in denen euer Programm fehl schlägt. Versucht euer Programm zu verbessern.



<a name="auf8"></a>

### Aufgabe 8

Wir betrachten noch einmal das Problem aus Aufgabe 6.

1. Verändert Euren Algorithmus so, dass die Piraten, nachdem sie die komplette Schatzspur aufgenommen haben, sie wieder in Form von Bojen ablegen.

1. Schön wäre es natürlich, wenn die Piraten nicht nur eine Spur aufnehmen könnten, sondern beim Zurücklaufen auch weitere abzweigende Spuren aufnehmen könnten (Piraten sind ja doch sehr gierig). Überlegt, wie ihr das Programm aus Aufgabenteil 1 verändern könnt, damit die Piraten auch auf ihrem Rückweg suchen. Entfernt als ersten Schritt die Markierungen auf dem Rückweg und sucht nach Punkten in Eurem Programm, an denen es sinnvoll wäre, nach weiteren Schätzen Ausschau zu halten. Hier ein Beispiel für einen unübersichtlichen, verzweigten Schatzpfad:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge7.jpg">



## Schleifen, Parameter und Variablen

**In diesen Aufgaben könnt Ihr davon ausgehen, dass die Felder keine Wellen oder Kraken enthalten. Außerdem könnt Ihr davon ausgehen, dass ggf. bestimmte Randzeilen und -spalten keine Schätze enthalten. Solltet Ihr später noch Zeit haben, könnt Ihr Eure Lösungen für Spezialfälle zusätzlich anpassen.**



<a name="auf9"></a>

### Aufgabe 9

Die Piraten sollen in einem leeren Feld ein Quadrat mit einer vorgegebenen Kantenlänge mit Bojen markieren. Achtet darauf, dass alle möglichen Kantenlängen funktionieren.

1. Malt zunächst nur ein Quadrat mit fest vorgegebener Seitenlänge.

1. Lasst die Piraten nun zuerst Schätze vor ihnen einsammeln und deren Anzahl als Kantenlänge für das Quadrat verwenden.



<a name="auf10"></a>

### Aufgabe 10

Die Piraten sollen überprüfen, ob zwei aufeinander folgende Zeilen gleich viele Schätze enthalten. Ist dies der Fall, so sollen sie in die erste Spalte der zweiten Zeile laufen. Sonst in die letzte Spalte der zweiten Zeile.



<a name="auf11"></a>

### Aufgabe 11

Schreibt ein Programm, welches alle Schätze in der Piratenwelt zählt und anschließend diese Zahl als aufeinanderfolgende Bojen ausgibt. Sollte für die Ausgabe eine Zeile nicht ausreichen, könnt Ihr mehrere Zeilen verwenden.

Geht davon aus, dass die Piraten am Anfang in der oberen linken Ecke stehen und nach rechts schauen.



<a name="auf12"></a>

### Aufgabe 12

Schreibt ein Programm, mit dessen Hilfe die Piraten herausfinden können, in welcher  Zeile sich die meisten Schätze auf dem Feld befinden. Als Ausgabe sollen die Piraten am Programmende in dieser Zeile stehen bleiben. Falls die Zeile mit den meisten Schätzen nicht eindeutig ist, können die Piraten in einer der Zeilen mit den meisten Schätzen stehen bleiben.

Vernachlässigt zunächst die Schätze in der ersten Zeile (nicht zählen!). Wer will kann sie später noch in den Algorithmus aufnehmen.



<a name="auf13"></a>

### Aufgabe 13 (Zusatzaufgabe)

Implementiert Aufgabe 9 ohne Schleifen zu verwenden nur mit Rekursion und Parameterübergabe.



<a name="auf14"></a>

### Aufgabe 14 (Zusatzaufgabe)

Ändert Aufgabe 9 so ab, dass das Quadrat um 45° verdreht dargestellt wird, also auf einer Spitze stehend.



<a name="auf15"></a>

### Aufgabe 15 (Zusatzaufgabe)

Bei dieser Aufgabe stehen die Piraten in der linken oberen Ecke des Feldes. Vor ihnen befinden sich Schätze und leere Felder, welche die binären Ziffern 1 und 0 repräsentieren. Diese binären Ziffern ergeben eine Binärzahl, welche zur Erkennung des Zahlenendes mit einer Welle abschließt. Die Piraten sollen diese Binärzahl (z.B. 11001) einlesen und in dem entsprechden Feld (hier z.B. das 25. Feld) eine Markierung ablegen. Um das Feld möglichst effizient zu finden, könnt ihr von einer festen Feldbreite ausgehen. Wer möchte, kann das Programm noch so erweitern, dass es mit beliebigen Feldgrößen selbständig klarkommt.



## Datenstrukturen



<a name="auf16"></a>

### Aufgabe 16

Die Piraten sollen überprüfen, ob zwei aufeinanderfolgende Zeilen jeweils das Spiegelbild voneinander sind. Ist dies der Fall, sollen sie ganz nach unten laufen. Sonst ganz nach oben. Folgendes Beispiel soll verdeutlichen, was mit "Spiegelbild voneinander" gemeint ist:

<img title = "Bild" width="30%" height="30%" src="/assets/challenge/challenge15.jpg">

Wie könnt Ihr Euer Programm ändern, damit nicht das Spiegelbild, sondern die Gleichheit der Zeilen überprüft wird?



<a name="auf17"></a>

### Aufgabe 17

Die Piraten sollen sich eine Zeile (bestehend aus Schätzen und freien Feldern) merken und eine andere Zeile suchen, die mit der ersten Zeile identisch ist. In dieser Zeile sollen sie stehen bleiben. Ist keine Zeile mit der ersten Zeile identisch, sollen sie zurück in die erste Zeile laufen.



<a name="auf18"></a>

### Aufgabe 18

Schreibt ein Programm, welches sich ähnlich wie das Programm aus Aufgabe 8.1 verhält. Allerdings sollen die Bojen nicht in umgekehrter Reihenfolge gelegt werden. Vielmehr sollen die Piraten an den Ausgangspunkt zurücksegeln und von dort die zunächst aufgesammelte Spur nachträglich mit Bojen markieren und am Ende der Spur stehen bleiben. Könnt Ihr die aufgesammelten Informationen auch dazu verwenden, zum Ausgangspunkt zurückzukehren, also hierzu nicht die Rekursion verwenden?



<a name="auf19"></a>

### Aufgabe 19 (Zusatzaufgabe)

Die Piraten sollen durch einen Spurcode gesteuert werden können. Der Spurcode ist eine Folge von Schätzen, Bojen und freien Feldern, welche durch eine Monsterwelle abgeschlossen wird. Damit der Spurcode auch mehr als 10 Zeichen lang sein kann, darf er auch mehrere Zeilen lang sein. Im Spurcode steht jedes freie Feld für einen Schritt nach vorne, jeder Schatz für eine Drehung nach links und jede Boje für eine Drehung nach rechts.

Schreibt ein Programm, welches die Piraten zunächst einen Spurcode (welcher direkt vor den Piraten beginnen sollte) einlesen lässt und dann die Spurfolge interpretiert, die Piraten also von der linken oberen Ecke aus an eine beliebige Feldposition navigieren lässt und dort abschließend eine Boje ablegt.



<a name="auf20"></a>

### Aufgabe 20 (Zusatzaufgabe)

Aufgabe 16 kann auch ohne Parameter und Datenstrukturen gelöst werden. Die Idee ist wieder die Verwendung von Rekursion.
