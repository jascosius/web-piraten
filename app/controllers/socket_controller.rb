# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q'

  include Preprocessor
  include InitializeCommunication
  include Communication
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
    language = message[:language].downcase
    code = message[:code]

    if code.length < 1000 and language.length < 50 and tracing_vars.length < 100 #TODO: shoud be the same as in the client

      tracing_vars.each do |e|
        if e.length > 100
          return
        end
      end

      code = preprocess_code(code, language, tracing_vars)

      #add EOF to show Wrapper the end of the code
      code += "\n#{$prefix}_EOF\n"

      start_simulation(code, tracing_vars)

    end

  end

  def stop
    puts 'stop'
    connection_store[:is_simulation_done] = true
  end


end