# Sprachspezifische Hilfe: Ruby
[Zurück](index "Hilfe")

####Die Programmiersprache Ruby unterstützt zur Zeit:  
-  Das Highlighten der aktuellen Zeile.  
-  Das Beobachten von Variablen.  
-  Das Überspringen von Codeblöcken.  
-  Das Setzen von Breakponits.  

##Einschränkungen:  
Bei Funktionen mit Rückgabewert muss **return** benutzt werden.  
Um eine korrekte Blockerkennung zu garantieren, dar nur ein Block pro Zeile geöffnet werden.  

##Funktionen

####Objekte
* :treasure
* :buoy
* :monster
* :wave
* (:border)

####Richtungen
* :front
* :back
* :left
* :right
* (:here)
<br><br>

###move()
Das Piratenschiff bewegt sich eine Feld in die aktuelle Bewegungsrichtung.  
**Akzeptierte Parameter:** : keine  

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
**Rückgabe:** das gefundene Objekt oder :nothing  

###take()
Nimmt das Objekt an der aktuellen Position auf.  
**Akzeptierte Parameter:** keine  
**Aufzunehmende Objekte:** :treasure, :buoy    

###break_ponit()
Setzt einen Breakpoint. Bei einer Simulation stoppt diese, sobald der Breakpoint erreicht ist.  
Von dieser Position kann dann weiter Simuliert oder Schrittweise durch den Code gegangen werden.  
**Akzeptierte Parameter:** :point setzt einen Breakpoint  
**Standardparameter:** :point  




