PREFIX = 'CkyUHZVL3q' #have to be the same as in initialize_communication
TIMEOUT = 60 #have to be the same as in initialize_communication
MAX_OPS = 10000 #the maximal counter of ops to execute
PORT = 12340 #have to be the same as in initialize_communication

#get the pepper to communicate with the server
#the server must have the same pepper
#IMPORTANT: The pepper is a secret
PEPPER_PATH_VM = '/pepper'