# Sprachspezifische Hilfe: Ruby
[Zurück](index "Hilfe")

####Für die Programmiersprache Ruby unterstützen wir zur Zeit:  
-  Das Highlighten der aktuellen Zeile.  
-  Das Beobachten von Variablen.  
-  Das Überspringen von Codeblöcken.  

##Funktionen

####Objekte
* :treasure
* :buoy
* :monster
* :wave
* (:border)  
* (:nothing)

####Richtungen
* :front
* :back
* :left
* :right
* (:here)
<br><br>

###move()
Das Piratenschiff bewegt sich eine Feld in die aktuelle Bewegungsrichtung.  
**Akzeptierte Parameter:**  keine  

###turn(Richtung)  
Dreht das Schiff mit der übergebenen Richtung.  
**Akzeptierte Parameter:** :left, :right, :back (180° Drehung)  
**Standardparameter:** :back  

###put(Objekt)
Platziert ein Objekt an der aktuellen Position.  
**Akzeptierte Parameter:** :treasure, :buoy  
**Standardparameter:** :buoy  

###look(Richtung)
Prüft, was sich auf dem Feld in der übergebenen Richtung befindet.  
**Akzeptierte Parameter:** alle Richtungen  
**Standardparameter:** :here  
**Rückgabe:** das gefundene Objekt 

###take()
Nimmt das Objekt an der aktuellen Position auf.  
**Akzeptierte Parameter:** keine  
**Aufzunehmende Objekte:** :treasure, :buoy    



##Einschränkungen:  
* Bei Funktionen mit Rückgabewert muss **return** benutzt werden.  
* Um eine korrekte Blockerkennung zu garantieren, darf in Zeilen, die mit einem  
mehrzeiligen String beginnen, kein Block und generell nur ein Block pro Zeile geöffnet werden.  



