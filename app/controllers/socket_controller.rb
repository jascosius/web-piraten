# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q'

  include Preprocessor
  include InitializeCommunication
  include Communication
  include ControlSimulation

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
    Perf::MeterFactory.instance.get(:socket).measure(:receive_code) { # performance

    tracing_vars = message[:vars]
    language = message[:language].downcase
    code = message[:code]

    #TODO: shoud be the same as in the client
    if Perf::MeterFactory.instance.get(:socket).measure(:code_length){code.length < 10000 and language.length < 50 and tracing_vars.length < 100}

      tracing_vars.each do |e|
        if e.length > 100
          return
        end
      end

      initialize_preprocessor(language)

      Perf::MeterFactory.instance.get(:socket).measure(:start_simulation){
        start_simulation(code, tracing_vars)
      }

    end

    } # performance
  end

  def stop
    puts 'stop'
    connection_store[:is_simulation_done] = true
  end

  def report
    [:socket, :vm].each do |name|
      puts "============== REPORTED METER (#{name}) =============="
      meter = Perf::MeterFactory.instance.get(name)
      puts meter.report_list_of_measures
      PERFORMANCE_LOGGER.report(meter, "ID:_#{connection.id}")
      Perf::MeterFactory.instance.clear_meter(name)
      puts "============================================"
    end
  end


end