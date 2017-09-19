# -*- coding: utf-8 -*-

"""
All the functions and enumerations required to program WebPiraten with Python. The commands
generated by this library depend on a prefix
"""

from enum import Enum



 #####
#     #  ####  #    # ###### #  ####
#       #    # ##   # #      # #    #
#       #    # # #  # #####  # #
#       #    # #  # # #      # #  ###
#     # #    # #   ## #      # #    #
 #####   ####  #    # #      #  ####

PREFIX = None
"""
The prefix is used to send proper messages to the server. It is configured through an initial
call to configure_prefix(...) below at the start of the user's program.
"""

def configure_prefix(prefix):
    """
    Called at the beginning of the user's program to setup the prefix the current VM will
    accept for commands.
    """
    global PREFIX

    # Only allow the prefix to be set once to keep users from messing with it
    if PREFIX is None:
        PREFIX = prefix



#     #
#     # ##### # #      # ##### # ######  ####
#     #   #   # #      #   #   # #      #
#     #   #   # #      #   #   # #####   ####
#     #   #   # #      #   #   # #           #
#     #   #   # #      #   #   # #      #    #
 #####    #   # ###### #   #   # ######  ####

def _issue_command(cmd):
    """
    Sends a command to the server that does not expect a reply. The command should be the part
    after 'PREFIX_', which is added automatically.
    """
    global PREFIX
    print(F"{PREFIX}_{cmd}")

def _issue_request(cmd):
    """
    Sends a command to the server and waits for a reply. The command should be the part after
    'PREFIX_', which is added automatically. The reply is returned as a String.
    """
    # Issue the command and return the reply
    _issue_command(cmd)
    return input()



#######
   #    #   # #####  ######  ####
   #     # #  #    # #      #
   #      #   #    # #####   ####
   #      #   #####  #           #
   #      #   #      #      #    #
   #      #   #      ######  ####

class Dir(Enum):
    """
    Directions used to indicate where to turn or to look. Not every direction can be used
    with every action. For example, it doesn't make sense to move 'here'.
    """

    FRONT = "front"
    BACK = "back"
    LEFT = "left"
    RIGHT = "right"
    HERE = "here"


class Obj(Enum):
    """
    Objects that can be encountered or put into the world. Not every object can be used with
    every action. For example, the ship is not allowed to put a new monster onto the sea. For
    safety reasons, mostly.
    """

    TREASURE = "treasure"
    BUOY = "buoy"
    MONSTER = "monster"
    WAVE = "wave"
    BORDER = "border"
    NOTHING = "nothing"

    @staticmethod
    def from_str(obj_str):
        """
        Checks if one of the objects is contained in the given string. If so, the object is
        returned. Otherwise, Obj.NOTHING is returned.
        """
        for obj in Obj:
            if obj.value in obj_str:
                return obj

        # We haven't found a match
        return Obj.NOTHING



######
#     # ###### #    #   ##   #    # #  ####  #    # #####
#     # #      #    #  #  #  #    # # #    # #    # #    #
######  #####  ###### #    # #    # # #    # #    # #    #
#     # #      #    # ###### #    # # #    # #    # #####
#     # #      #    # #    #  #  #  # #    # #    # #   #
######  ###### #    # #    #   ##   #  ####   ####  #    #

def move():
    """
    Move the boat forward with respect to the direction it's currently facing.
    """
    _issue_command("move")

def turn(direction=Dir.BACK):
    """
    Turns the ship according to the given direction. Valid directions are LEFT, RIGHT,
    and BACK. Other directions cause an error.
    """
    if direction in (Dir.LEFT, Dir.RIGHT, Dir.BACK):
        _issue_command(F"turn_{direction.value}")
    else:
        # TODO Error handling
        pass

def look(direction=Dir.HERE):
    """
    Looks in a given direction and returns the object found there.
    """
    if direction in Dir:
        # Issue the command and let the Obj enumeration find out which object is
        # in the reply
        reply = _issue_request(F"?_look_{direction.value}")
        return Obj.from_str(reply)
    else:
        # TODO Error handling
        pass

def put(obj=Obj.BUOY):
    """
    Puts an object onto the field the ship currently resides on. Valid objects are
    BUOY and TREASURE.
    """
    if obj in (Obj.TREASURE, Obj.BUOY):
        _issue_command(F"put_{obj.value}")
    else:
        # TODO Error handling
        pass

def take():
    """
    Take whatever is on the current field.
    """
    _issue_command("take")