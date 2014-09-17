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
      connection_store[:incoming] = receive_start # performance

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
        start_preprocessor = Time.now
        @preprocessor = Preprocessor.new(language,code,tracing_vars)
        PERFORMANCE_LOGGER.store :start_preprocessor, start_preprocessor, Time.now
        start_simulation_perf = Time.now
        start_simulation(tracing_vars)
        PERFORMANCE_LOGGER.store :start_simulation, start_simulation_perf, Time.now

      end

      PERFORMANCE_LOGGER.store :receive_code, receive_start, Time.now
    end
  end

  def stop
    puts 'stop'
    connection_store[:is_simulation_done] = true
  end

  def ping
    start = Time.now
    send_message :pong, ''
    PERFORMANCE_LOGGER.store :ping, start, Time.now
  end

  def report
    PERFORMANCE_LOGGER.save
    puts 'Saving performance logs!'
  end


end
