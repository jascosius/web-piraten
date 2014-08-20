# https://gist.github.com/kinopyo/1278448
require 'logger'

class PerformanceLogger < Logger
  def format_message(severity, timestamp, progname, msg)
    "#{timestamp.to_formatted_s(:db)} #{severity} || #{msg}"
  end
  def report(meter, id = '')
    return unless PERFORMANCE_TEST

    for report in meter.report_simple
      # puts "report!"
      if report != nil and report != ''
        # puts "report!!"
        # puts report
        if id != ""
          report = "#{id} || #{report}"
        end
        info(report)
        # puts "report!!!"
      end
    end
    info "=================== #{id} ===============================\n"
  end
end

logfile = File.open("#{Dir.pwd}/log/performance.log", 'a')  # create log file
logfile.sync = true  # automatically flushes data to file
PERFORMANCE_LOGGER = PerformanceLogger.new(logfile)  # constant accessible anywhere