if ENV['PERFORMANCE_TEST']
  PERFORMANCE_TEST = true
  puts 'PERFORMANCE_TEST environment variable is set, activating method profiling'
else
  PERFORMANCE_TEST = false
  puts 'Server starts without method profiling, set the environment variable PERFORMANCE_TEST=true to enable it.'
end

class PerformanceLogger

  def initialize(name)
    @name = name
    @log = []
  end

  def track(id, name, sec)
    return unless PERFORMANCE_TEST
    @log.push "#{id},#{name},#{(sec*1000)}"
  end

  def save
    return unless PERFORMANCE_TEST
    puts @log
    File.open("#{Rails.root}/log/performance_#{@name}.log", 'w') do |file|
      @log.each do |entry|
        file.puts entry
      end
    end

  end
end

PERFORMANCE_LOGGER = PerformanceLogger.new 'rails'
