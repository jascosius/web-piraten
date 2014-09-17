# -*- encoding : utf-8 -*-
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
    @timings = Hash.new
  end

  def track(id, name, sec)
    return unless PERFORMANCE_TEST
    @log.push "#{id},#{name},#{(sec*1000)}"
  end

  def store(name, start_time, end_time)
    return unless PERFORMANCE_TEST
    ms = (end_time-start_time)*1000
    puts "storing #{name}, #{ms}"
    @timings[name] ||= []
    @timings[name] << ms
    # puts "saved #{@timings[name]}"
  end

  def save
    return unless PERFORMANCE_TEST
    puts @log
    File.open("#{Rails.root}/log/performance_#{@name}.log", 'w') do |file|
      # @log.each do |entry|
      #   file.puts entry
      # end
      @timings.each do |name, times|
        puts "saving! #{name}"
        file.puts "\n#{name}"
        sum = 0

        # puts "sum #{sum}"
        times.each do |ms|
          file.puts ms
          puts ms
          sum += ms
        end

        # puts "sum, after #{sum}"
        avg = (sum.to_f/times.length)
        # puts "avg #{avg}"
        file.puts "avg: #{avg}"
      end
    end
    @log.clear
    @timings.clear

  end
end

PERFORMANCE_LOGGER = PerformanceLogger.new 'rails'
