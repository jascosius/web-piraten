# -*- encoding : utf-8 -*-
require 'socket'
require 'open3'
require 'fileutils'
require 'json'
require 'thread'

#ips that can connect to the vm
WHITELIST_IPS = ['134.245.252.93'] #chevalblanc

PREFIX = 'CkyUHZVL3q' #have to be the same as in the socket_controller
TIMEOUT = 20 #have to be the same as in the socket_controller
MAX_OPS = 10000 #the maximal counter of ops to execute
PORT = 12340 #have to be the same as in the socket_controller

if ARGV[0] == 'production'
  puts 'Starting VM in production-mode.'
  DEVELOPMENT = false
else
  puts 'Starting VM in development-mode.'
  DEVELOPMENT = true
end

if ARGV.length > 1 and ARGV[1] == 'performance'
  puts 'Performance mode set as active'
  PERFORMANCE_TEST = true
else
  puts 'Performance mode set as inactive'
  PERFORMANCE_TEST = false
end

#thread to kill the execution after a while
def initialize_timeout(client)
  Thread.start(Thread.current) do |thread|
    sleep(TIMEOUT)
    if thread.alive?
      puts msg = "\n#{PREFIX}_enderror_Das Programm hat zu lange gebraucht."
      client.puts msg
      thread.kill
    end
  end
end

#wait for incoming commands from the server
def get_commands(client, functions)
  # Thread to handel incomming messages

  loop do
    msg = client.gets.chomp
    puts "Incoming: #{msg}"

    msg = JSON.parse(msg)

    Thread.start do
      msg.each do |item|
        search_and_execute_function(functions, item.keys[0], item.values[0])
      end
    end
  end
end

def handle_queue_functions(queue)
  Thread.start do
    loop do
      queue.pop.call
    end
  end
end

def search_and_execute_function(functions, name, hash)
  functions[name.to_sym].call(hash)
end

def response(hash, shared)
  if shared[:stdin]
    shared[:stdin].puts hash['value']
  end
end

def exit(hash, client, shared)
  puts 'exit'
  if hash['successful']
    end_msg = "\n#{PREFIX}_end"
  else
    end_msg = "\n#{PREFIX}_enderror_#{hash['message']}"
  end

  if PERFORMANCE_TEST
    exit_time = Time.now
    start_time = shared[:start_time]

    diff = exit_time - start_time
    client.puts timing = "\n#{PREFIX}_timings_vmVerarbeitung_#{diff}"
    puts timing
  end

  puts end_msg
  client.puts end_msg
end

def write_file(hash, dir)
  open("#{dir}/#{hash['filename']}", 'w+') do |file|
    File.write file, hash['content']
    File.chmod(hash['permissions'], file)
  end
end

def execute(hash, client, dir, shared)
  command = hash['command'].gsub('$LIB$', Dir.pwd + '/lib').gsub('$PATH$', dir)

  changeuser = 'sudo -u sailor '
  if DEVELOPMENT or hash['permissions'] == 'read-write'
    changeuser = ''
  end

  puts command = "cd #{dir} && " + changeuser + command
  Open3.popen3(command) do |stdin, stdout, stderr|
    stdout.sync = true
    stdin.sync = true
    shared[:stdin] = stdin

    Thread.start do
      handle_stderr(stderr, hash['stderr'], shared)
    end
    handle_stdout(client, stdout, hash['stdout'], shared)
  end
end

def handle_stdout(client, stdout, tag, shared)
  counter = 0
  loop do
    if stdout.eof?
      diff = Time.now - shared[:incoming_file]
      #print errormessages at last
      if shared[:err]
        shared[:err].each_line do |line|
          puts line

          client.puts timing = "\n#{PREFIX}_timings_vmIncomingFile_#{diff}"
          client.puts line
        end
      end

      break
    elsif counter > MAX_OPS
      #tell the client that the execution has finished with errors
      puts msg = "#{PREFIX}_enderror_Die maximale Anzahl an Operationen wurde erreicht."
      client.puts msg
      break
    end
    counter += 1
    line = stdout.readline

    unless line.chomp.empty?
      unless tag == false || line.start_with?(PREFIX)
        line = "#{PREFIX}_print_#{tag}_#{line}"
      end
      puts line
      client.puts line
    end
  end
end

def handle_stderr(stderr, tag, shared)
  loop do
    line = stderr.readline

    unless line.chomp.empty?
      if tag
        #print errormessages at last
        shared[:err] ||= ''
        shared[:err] = shared[:err] + "\n#{PREFIX}_print_#{tag}_#{line}"
      end
    end
  end
end


server = TCPServer.new PORT
loop {
  Thread.start(server.accept) do |client| #spawn new process for a new client

    #just accept connections from whitelisted_ips
    unless DEVELOPMENT
      unless WHITELIST_IPS.include? client.peeraddr[3]
        puts "The IP '#{client.peeraddr[3]}' is not in the whitelist."
        return
      end
    end

    shared = {:start_time => Time.now}
    queue = Queue.new

    temp = '/codetemp'
    if DEVELOPMENT
      temp = '/tmp'
    end

    dir = "#{temp}/session_#{Time.now.to_i}_#{rand(1000000)}"

    thr = Thread.start {
      initialize_timeout(client)
      thread = Thread.current

      Dir.mkdir(dir, 0755)

      functions = {:response => lambda { |hash| response(hash, shared) }, #execute immediate
                   :stop => lambda { |_| puts 'stop'; thread.kill },
                   :write_file => lambda { |hash| shared[:incoming_file]= Time.now
                   queue.push( lambda {write_file(hash, dir)} )}, #add to queue
                   :execute => lambda {|hash| queue.push( lambda {execute(hash, client, dir, shared)} )},
                   :exit => lambda { |hash| queue.push( lambda {exit(hash, client, shared)} )}}

      handle_queue_functions(queue)
      get_commands(client, functions)
    }

    thr.join #wait for execution to finish or stopped by timeout

    FileUtils.rm_r dir #clean up

  end
}.join
