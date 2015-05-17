ALTERNATIVE_CONF_PATH_PRO = File.expand_path('env_conf/application.conf') #Alternativ file for this config. If not exist use this file
ALTERNATIVE_CONF_PATH_DEV = File.expand_path('env_conf/application.conf') #Alternativ file for this config. If not exist use this file

if Rails.env.production?
  if ALTERNATIVE_CONF_PATH_PRO and File.exist?(ALTERNATIVE_CONF_PATH_PRO + '.rb')
    require ALTERNATIVE_CONF_PATH_PRO
  else
    #Settings for production
    VM_TIMEOUT = 60 #timeout time in sec for the user code to execute, change in vm.conf as well
    VM_PORT = 11111 #port to connect to the vm for the code execution
    VM_HOST = 'server_ip' #host to connect to the vm for the code execution
    VM_PREFIX = 'abcdefghijk' #prefix to seperate user output and commands (use ASCII characters for best support)

    #pepper to communicate with the vm (symmetric key)
    #use a random string with at least 32 characters
    #the vm must have the same pepper
    #IMPORTANT: The pepper is a secret
    PEPPER_PATH_RAILS = Dir.home + '/pepper' #path to pepper
  end
else
  if ALTERNATIVE_CONF_PATH_DEV and File.exist?(ALTERNATIVE_CONF_PATH_DEV + '.rb')
    require ALTERNATIVE_CONF_PATH_DEV
  else
    #Settings for development
    VM_TIMEOUT = 60 #timeout time in sec for the programm to execute, change in the vm.conf as well
    VM_PORT = 11111 #port to connect to the vm
    VM_HOST = 'localhost' #host to connect to the vm
    VM_PREFIX = 'abcdefghijk' #prefix to seperate user output and commands
  end
end