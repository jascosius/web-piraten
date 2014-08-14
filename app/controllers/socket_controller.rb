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
    language = message[:language]
    code = message[:code]

    #TODO: shoud be the same as in the client
    if Perf::MeterFactory.instance.get(:socket).measure(:code_length){code.length < 10000 and language.length < 50 and tracing_vars.length < 100}

      Perf::MeterFactory.instance.get(:socket).measure(:tracing_vars_length){
      tracing_vars.each do |e|
        if e.length > 100
          return
        end
      end
      }

      Perf::MeterFactory.instance.get(:socket).measure(:preprocess_code){
        code = preprocess_code(code, language, tracing_vars)
      }

      #add EOF to show Wrapper the end of the code
      code += "\n#{$prefix}_EOF\n"

      Perf::MeterFactory.instance.get(:socket).measure(:start_simulation) {
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
    puts "============== REPORTED METER =============="
    meter = Perf::MeterFactory.instance.get(:socket)
    puts meter.report_list_of_measures
    PERFORMANCE_LOGGER.report(meter)
    puts "============================================"

  end


end