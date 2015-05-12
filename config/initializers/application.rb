if Rails.env.production?
  require '../../env_conf/application'
  #Settings for production
  #VM_TIMEOUT = 60 #timeout time for the programm to execute, change in the vm as well
  #VM_PORT = 12340 #port to connect to the vm
  #VM_HOST = 'localhost' #host to connect to the vm
  #VM_PREFIX = 'CkyUHZVL3q'
  #PEPPER_PATH_RAILS = Dir.home + '/.webpiraten-pepper' #path to pepper
else
  #Settings for development
  VM_TIMEOUT = 60 #timeout time for the programm to execute, change in the vm as well
  VM_PORT = 12340 #port to connect to the vm
  VM_HOST = 'localhost' #chevalblanc.informatik.uni-kiel.de' #host to connect to the vm #localhost
  VM_PREFIX = 'CkyUHZVL3q'
end