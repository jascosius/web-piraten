require 'rubyperf'
require 'performance_logger'

if ENV['PERFORMANCE_TEST']
  puts 'PERFORMANCE_TEST environment variable is set, activating method profiling'
  Perf::MeterFactory.instance.get(:socket) # create singleton
else
  puts 'Server starts without method profiling, set the environment variable PERFORMANCE_TEST=true to enable it.'
  Perf::MeterFactory.instance.set_factory_options(:noop => true)
end