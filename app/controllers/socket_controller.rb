# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController

  require 'preprocessor/preprocessor'
  require 'simulation/initialize_communication'
  require 'simulation/communication'
  require 'simulation/control_simulation'
  require 'simulation/grid'
  require 'simulation/ship'
  require 'simulation/packet_handler'

  $prefix = 'CkyUHZVL3q'

  def initialize_session
    puts 'initializing session with client'
  end

  def client_connected
    puts 'client successfully connected!'
  end

  def client_disconnected
    puts 'client disconnected!'
  end

  # test events for the remote control buttons

  def receive_code
    Thread.start do
      receive_start = Time.now # performance

      tracing_vars = message[:vars]
      language = message[:language].downcase
      code = message[:code]

      #TODO: shoud be the same as in the client
      if code.length < 10000 and language.length < 50 and tracing_vars.length < 100

        tracing_vars.each do |e|
          if e.length > 100
            return
          end
        end

        @preprocessor = Preprocessor.new(language,code,tracing_vars)

        start_simulation(tracing_vars)

      end

      PERFORMANCE_LOGGER.track(connection.id, :receive_code, Time.now - receive_start)
    end
  end

  def stop
    puts 'stop'
    connection_store[:is_simulation_done] = true
  end

  def report
    PERFORMANCE_LOGGER.save
    puts 'Saving performance logs!'
  end


end
