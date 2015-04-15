# Sprachspezifische Hilfe: Java
[Zurück](index "Hilfe") 

##Funktionen

####Objekte
* Item.TREASURE
* Item.BUOY
* Item.MONSTER
* Item.WAVE
* (Item.BORDER)
* (Item.NOTHING)

####Richtungen
* Direction.FRONT
* Direction.BACK
* Direction.LEFT
* Direction.RIGHT
* (Direction.HERE)
<br><br>

###move()
Das Piratenschiff bewegt sich eine Feld in die aktuelle Bewegungsrichtung.  
**Akzeptierte Parameter:** keine  

###turn(Richtung)  
Dreht das Schiff mit der übergebenen Richtung.  
**Akzeptierte Parameter:** Direction.LEFT, Direction.RIGHT, Direction.BACK (180° Drehung)  
**Standardparameter:** Direction.BACK

###put(Objekt)
Platziert ein Objekt an der aktuellen Position.  
**Akzeptierte Parameter:** Item.TREASURE, Item.BUOY  
**Standardparameter:** Item.BUOY  

###look(Richtung)
Prüft, was sich auf dem Feld in der übergebenen Richtung befindet.  
**Akzeptierte Parameter:** alle Richtungen  
**Standardparameter:** Direction.HERE  
**Rückgabe:** das gefundene Objekt 

###take()
Nimmt das Objekt an der aktuellen Position auf.  
**Akzeptierte Parameter:** keine  
**Aufzunehmende Objekte:** Item.TREASURE, Item.BUOY   


##Einschränkungen:  
Die aktuelle Zeile wird während der Ausführung nicht markiert.  





