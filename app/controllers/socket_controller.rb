# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q'
  #@@timeout = 5 #timeout time for the programm to execute
  #@@port = 12340 #port to connect to the vm
  #@@host = 'localhost' #host to connect to the vm
  # @@id moved to initialize_communiation

  include Preprocessor
  include InitializeCommunication
  include Communication
#  include GridSimulation
  include ControlSimulation

  def initialize_session
    puts 'new_event was called'
  end

  def client_connected
    puts 'client connected!'
  end

  def client_disconnected
    puts 'client disconnected!'
  end

  # test events for the remote control buttons

  def receive_code

    tracing_vars = message[:vars]
    language = 'Ruby'
    code = preprocess_code(message[:code], language, tracing_vars)

    #add EOF to show Wrapper the end of the code
    code += "\n#{$prefix}_EOF\n"

    start_simulation(code, tracing_vars)

  end

  def stop
    puts 'stop'
    connection_store[:is_simulation_done] = true
  end


  #####initalize_communication TODO: move to initalize_communication.rb



  ###communication TODO: move to communication.rb

end