
def saveReport(meter)
  reports = meter.report_html
  reportsimple = meter.report_simple
  File.open('performance.html', 'w') { |file|
    reports.each do |r|
      unless r == nil or r == ''
        file.write(r)
        file.write("<br /> \n")
      end
    end
  }
  File.open('performance.txt', 'w') { |file|
    reportsimple.each do |r|
      unless r == nil or r == ''
        file.write(r)
        file.write("\n")
      end
    end
  }
end

if ENV['PERFORMANCE_TEST']
  puts 'PERFORMANCE_TEST environment variable is set, activating method profiling'
  Perf::MeterFactory.instance.get # create singleton

  at_exit do
    puts 'Shutting down!'
    report = Perf::MeterFactory.instance.get(:socket)
    saveReport report
    puts 'saved performance report'
    puts report
  end
else
  puts 'Server starts without method profiling, set the environment variable PERFORMANCE_TEST=true to enable it.'
  Perf::MeterFactory.instance.set_factory_options(:noop => true)
end