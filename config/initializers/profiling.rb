require 'rubyperf'
require 'performance/performance_logger'
if ENV['PERFORMANCE_TEST']
  PERFORMANCE_TEST = true
  puts 'PERFORMANCE_TEST environment variable is set, activating method profiling'
  Perf::MeterFactory.instance.get(:socket) # create singleton
else
  PERFORMANCE_TEST = false
  puts 'Server starts without method profiling, set the environment variable PERFORMANCE_TEST=true to enable it.'
  Perf::MeterFactory.instance.set_factory_options(:noop => true)
end