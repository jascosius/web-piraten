
#Hilfe<img title = "Lernoberfläche" align="right" width="60%" height="60%" src="/assets/help/start.png">

####Sprachspezifische Hilfe findest du hier:
* [Erlang](erlang "Erlangspezifische Hilfe")
* [Java](java "Javaspezifische Hilfe")
* [Python](python "Pythonspezifische Hilfe")
* [Ruby](ruby "Rubyspezifische Hilfe")

### Allgemeine Hilfe:
1. [Allgemein](#allg)
1. [Navigation](#nav)
2. [Spielfeld](#feld)
3. [Eingabe des Codes](#eingabe)
4. [Setzen von Breakpoints](#breakp)
4. [Simulation](#sim)
5. [Variablen beobachten](#variablen)
6. [Ausgabekonsole](#kons)
7. [Laden/Speichern](#laden)<a name="allg"></a>
<br>
<br><br>

### Allgemein

Auf der **Lernoberfläche** hast du die Möglichkeit, eigenen
Code zu schreiben und simulieren zu lassen.
**Ziel** ist es, das **Piratenschiff** über das Spielbrett zu steuern,
mit **Objekten** zu interagieren und so Aufgaben zu lösen.
Unter der **sprachspezifischen Hilfe** (s.o.) findest du nähere Informationen
zu vordefinierten Funktionen und angebotenen Sonderfunktionen.
<br><br>
<a name="nav"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>

### Navigation

Über die **Navigationsleiste** kannst du dich durch die Seite navigieren.
Unter **Lernen** kannst du eine Sprache auswählen und kommst dann in die Lernoberfläche.
**Hilfe** führt dich zu dieser Seite, wo du nähere Informationen über die Lernoberfläche findest.
Unter **Webpiraten** findest du unsere Startseite.
<br><br>
<a name="feld"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>

###Spielfeld

Auf dem **Spielfeld** kannst du die Simulation *live* verfolgen.
<br>

> ####Objekte
> ![Piratenschiff: ]( /assets/frank/ship.gif "Piratenschiff") Die Simulation wird aus der Sicht des **Piratenschiffes** durchgeführt.
![Kraken: ]( /assets/frank/monster.gif "Kraken") Wenn das Schiff versucht auf einen **Kraken** zu fahren, wird die Simulation sofort abgebrochen.
![Welle: ]( /assets/frank/wave.gif "Welle") Versucht das Schiff auf eine **Welle** zu fahren, bekommst du eine Warnung in der Konsole und die Operation wird nicht durchgeführt.
![Schatz: ]( /assets/frank/treasure.gif "Schatz") Ein **Schatz** kann vom Piratenschiff aufgenommen und gesetzt werden.
![Boje: ]( /assets/frank/buoy.gif "Boje") Eine **Boje** kann vom Piratenschiff aufgenommen und *nur* vom Piratenschiff gesetzt werden.

Außerhalb der [Simulation](#sim) kannst du die Objekte *Schatz*, *Welle* und *Kraken* dem Spielfeld hinzufügen. Hierzu musst  <img title = "Auswahl der Objekte" align="right" src="/assets/help/objects.png">
du einfach das einzufügene Objekt in der Leiste auswählen und dieses dann per **linkem Mausklick** in das Feld einfügen.
Du kannst auch komfortabel mit den Tasten **1, 2** und **3** das passende Objekt wählen und durch Halten der **linken Maustaste**
mehrere Objekte in einer Linie ziehen.
Außerdem kannst du durch Halten der **linken Maustaste** auf das Piratenschiff dieses auf dem Spielbrett verschieben.
Mit einem **Rechtsklick** kannst du entweder die Ausrichtung des Schiffes ändern oder andere Objekte entfernen.
<br><br>
<a name="eingabe"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>


###Eingabe des Codes <img title = "Eingabefeld" align="right" src="/assets/help/codemirror.png">

Im Eingabefeld kannst du, wenn keine Simulation läuft, selbst Code eingeben.
Mit dem hier eingegebenen Code kannst du das Piratenschiff über das Spielfeld lenken, mit den Objekten interagieren, um damit deine Aufgaben zu erfüllen.
Die Sprachspezifischen Befehle, mit denen du das Schiff steuern kannst, findest du in der sprachspezifischen Hilfe (s.o.).
![]( /assets/help/big.png "Vollbildmodus") Hiermit kannst du zwischen normalem Eingabemodus und Vollbildmodus wechseln.
<br><br>
<a name="breakp"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>


###Setzen von Breakpoints

Wenn du das Eingabefeld im Fokus hast (z.B gerade Code schreibst), gibt es die Möglichkeit,  <img title = "Breakpoint" hspace = "150" align="right"    src="/assets/help/breakpoint.png">
einen Breakpoint zu setzen. Die Simulation wird dann vor diesem Breakpoint stoppen,
so dass du die Simulation bis zu den interessanten Codepassagen in hoher Geschwindigkeit
"überfliegen" und dann schrittweise den Code untersuchen kannst.
**Um einen Breakpoint zu setzen, benutzt du die Tastenkombination "Ctrl-b" (Strg-b).**
Eine so markierte Zeile wird durch einen roten Punkt gekennzeichnet. Mit "Ctrl-b" kannst
du auch einen schon vorhandenen Breakpoint löschen.
<br><br>
<a name="sim"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>






###Simulation

Sobald du auf **Ausführen** drückst, wird die Simulation deines Codes gestartet.
Sie läuft dann automatisch ab, wobei du über den **Geschwindigkeitsregler** die <img title = "Start der Simulation" align="right" src="/assets/help/simulation1.png">
Simulationsgeschwindigkeit selbst auswählen kannst. Anstatt die Simulation zu starten,
kannst du auch direkt in die **schrittweise Ausführung** wechseln. <img title = "Stop-Button" hspace = "200" align="right" src="/assets/help/stop.png">
Mit dem **Stop-Button** kannst du die Simulation anhalten.
Hast du die Simulation gestoppt oder sie schrittweise gestartet, kannst du entwerder **schrittweise** durch den Code gehen,<img title = "Simulationsoptionen" align="right" src="/assets/help/simulation2.png">
die Simulation **weiter ausführen** lassen oder das Spielfeld **zurücksetzen** und in den
Programmiermodus zurückkehren. Außerdem kannst du, sollte diese Funktion von der ausgewählten
Sprache unterstützt werden, aus dem aktuellen Codeblock **springen**.
Befinden sich Fehler in deinem Programm, werden diese in der Konsole (s.u.) rot ausgegeben, und die Programmausführung beendet.
<br><br>
<a name="variablen"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>

###Variablen beobachten <img title = "Variablen beobachten" align="right" src="/assets/help/variable2.png">
Um deinen Code noch besser untersuchen zu können, gibt es, je nach gewählter Sprache, die Möglichkeit,
den Wert von **Variablen** zu verfolgen. Hierzu markiere einfach eine Variable, die du beobachten möchtest
mit einem Doppelklick. Daraufhin findest du diese in der Liste der zu  beobachtenden Variablen.
Benötigst du sie nicht mehr, kannst du sie, mit eine Klick auf das Kreuz, auch wieder entfernen. <img title = "Variablen beobachten" align="right" src="/assets/help/variable1.png">
Während der Simulation wird der Wert dieser Variablen, ab ihrer Definition, *live* unter
dem Eingabefeld angezeigt.
<br><br>
<a name="kons"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>

###Konsole <img title = "Ausgabekonsole" align="right" src="/assets/help/console1.png">
In der Ausgabekonsole werden **Warnungen** und **Fehlermeldungen** angezeigt.
Fehlermeldungen sind rot unterlegt und beenden die Programmausführung.
Warnungen hingegen sind gelb brechen die Simulation nicht ab.
Fährt das Schiff zum Beispiel gegen eine Welle, wird eine Warnung ausgegeben
und das Schiff bewegt sich nicht, die Simmulation geht aber weiter.
<br><br>
<a name="laden"></a>
[Nach oben](index "springe zum Anfang der Seite")
<br><br>

###Laden/Speichern ![]( /assets/help/save1.png "Laden und Speichern")<img title = "Laden und Speichern" align="right" src="/assets/help/save2.png">
Über den Laden-/ Speicher-Button hast du die Möglichkeit,
den Code oder das Spielfeld bzw. die Spielwelt zu speichern
oder zu laden.
<br><br>
[Nach oben](index "springe zum Anfang der Seite")

