ALTERNATIVE_CONF_PATH_PRO = File.expand_path('vm.mine.conf') #Alternativ file for this config. If not exist use this file (without .rb)
ALTERNATIVE_CONF_PATH_DEV = File.expand_path('../../env_conf/vm.conf') #Alternativ file for this config. If not exist use this file (without .rb)

if ENV.fetch('mode') == 'production'
  if ALTERNATIVE_CONF_PATH_PRO and File.exist?(ALTERNATIVE_CONF_PATH_PRO + '.rb')
    require ALTERNATIVE_CONF_PATH_PRO
  else
    PREFIX = 'abcdefghjk' #has to be the same as in application_conf
    TIMEOUT = 120 #has to be the same as in application_conf
    MAX_OPS = 10000 #the maximal counter of ops to execute
    PORT = 11111 #port for ingoing connections, has to be the same as in application.conf

    CODE_DIR = '/codetemp' #folder to store the user code for execution
    LIB_DIR = 'lib' #folder which provides additional libaries for the programming language, relative from working dir

    READ_ONLY_USER = 'sailor' #User with read-only access to everything which executes the user code
    READ_WRITE_USER = 'builder' #User with write to code path which compiles user code

    #pepper to communicate with the server (symmetric key)
    #the server must have the same pepper
    #IMPORTANT: The pepper is a secret
    PEPPER_PATH_VM = '/pepper' #path to the pepper

    WHITELIST_IPS = ['server_ip'] #ips that can connect to the vm

    MSG_TO_LONG='Maximal execution time reached.'
    MSG_MAX_OPS='Maximal operations reached.'
  end
else
  if ALTERNATIVE_CONF_PATH_DEV and File.exist?(ALTERNATIVE_CONF_PATH_DEV + '.rb')
    require ALTERNATIVE_CONF_PATH_DEV
  else
    PREFIX = 'abcdefghijk' #has to be the same as in application.conf
    TIMEOUT = 120 #has to be the same as in application.conf
    MAX_OPS = 10000 #the maximal counter of ops to execute
    PORT = 11111 #port for ingoing connections, has to be the same as in application.conf

    CODE_DIR = '/tmp'
    LIB_DIR = 'lib' #relative from working dir

    MSG_TO_LONG='Maximal execution time reached.'
    MSG_MAX_OPS='Maximal operations reached.'
  end

end