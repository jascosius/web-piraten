# Sprachspezifische Hilfe: Erlang
[Zurück](index "Hilfe")

####Für die Programmiersprache Erlang unterstützen wir zur Zeit:  
-  Das Highlighten der aktuellen Zeile.  
-  Das Beobachten von Variablen.  
-  Das Überspringen von Codeblöcken.  

##Funktionen

####Objekte
* treasure
* buoy
* monster
* wave
* (border)  
* (nothing)

####Richtungen
* front
* back
* left
* right
* (here)
<br><br>

###move()
Das Piratenschiff bewegt sich ein Feld in die aktuelle Bewegungsrichtung.
**Akzeptierte Parameter:**  keine  

###turn(Richtung)  
Dreht das Schiff mit der übergebenen Richtung.  
**Akzeptierte Parameter:** left, right, back (180° Drehung)  
**Standardparameter:** back  

###puts(Objekt)
Platziert ein Objekt an der aktuellen Position.  
**Akzeptierte Parameter:** treasure, buoy  
**Standardparameter:** buoy  

###look(Richtung)
Prüft, was sich auf dem Feld in der übergebenen Richtung befindet.  
**Akzeptierte Parameter:** alle Richtungen  
**Standardparameter:** here  
**Rückgabe:** das gefundene Objekt 

###take()
Nimmt das Objekt an der aktuellen Position auf.  
**Akzeptierte Parameter:** keine  
**Aufzunehmende Objekte:** treasure, buoy    

###break()
Setzt einen Breakpoint. Bei einer Simulation stoppt diese, sobald der Breakpoint erreicht ist.  
Von dieser Position kann dann weiter Simuliert oder Schrittweise durch den Code gegangen werden.  
**Akzeptierte Parameter:** :point setzt einen Breakpoint  
**Standardparameter:** :point  

##Einschränkungen:  
* Mehrzeilige Strings und Atome sollten nicht genutzt werden. Sie werden nicht korrekt erkannt und führen zu falschen Auswertungen.
