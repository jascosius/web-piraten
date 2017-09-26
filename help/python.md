# Sprachspezifische Hilfe: Python
[Zurück](index "Hilfe")

##Objekte

* `Obj.TREASURE`
* `Obj.BUOY`
* `Obj.MONSTER`
* `Obj.WAVE`
* (`Obj.BORDER`)
* (`Obj.NOTHING`)


##Richtungen

* `Dir.FRONT`
* `Dir.BACK`
* `Dir.LEFT`
* `Dir.RIGHT`
* (`Dir.HERE`)


##Funktionen

###move()

Das Piratenschiff bewegt sich ein Feld in die aktuelle Bewegungsrichtung.

**Akzeptierte Parameter:**  keine


###turn(Richtung)

Dreht das Schiff mit der übergebenen Richtung.

**Akzeptierte Parameter:** `Dir.LEFT`, `Dir.RIGHT`, `Dir.BACK` (Drehung um 180°)

**Standardparameter:** `Dir.BACK`


###put(Objekt)

Platziert ein Objekt an der aktuellen Position.

**Akzeptierte Parameter:** `Obj.TREASURE`, `Obj.BUOY`

**Standardparameter:** `Obj.BUOY`


###look(Richtung)

Prüft, was sich auf dem Feld in der übergebenen Richtung befindet.

**Akzeptierte Parameter:** alle Richtungen (`Dir.FRONT`, `Dir.BACK`, `Dir.LEFT`, `Dir.RIGHT`, `Dir.HERE`)

**Standardparameter:** `Dir.HERE`

**Rückgabe:** das gefundene Objekt


###take()

Nimmt das Objekt an der aktuellen Position auf.

**Akzeptierte Parameter:** keine

**Aufzunehmende Objekte:** `Obj.TREASURE`, `Obj.BUOY`
