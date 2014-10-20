# -*- encoding : utf-8 -*-
#runs on the vm to execute the user code
#the vm has to be secure, so that the user can't take over the vm

require 'socket'
require 'open3'
require 'fileutils'
require 'json'
require 'thread'
require 'digest'

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

#get the pepper to communicate with the server
#the server must have the same pepper
#IMPORTANT: The pepper is a secret
pepper_path = '/pepper'
if DEVELOPMENT
  PEPPER = ''
  puts 'The pepper is empty.'
else
  pepper = ''
  begin
    File.open(pepper_path) do |file|
      pepper = file.read
    end
  rescue
    $stderr.puts "There is no pepper in '#{pepper_path}'"
    exit 1
  else
    if pepper.length < 32
      $stderr.puts "The pepper '#{pepper}' is not long enough."
      exit 1
    end
    PEPPER = pepper
    end
    puts "The hash of the pepper is '#{Digest::SHA256.hexdigest(PEPPER)}'."
end

#thread to kill the execution after a while
def initialize_timeout(client)
  Thread.start(Thread.current) do |thread|
    sleep(TIMEOUT)
    if thread.alive?
      msg = "\n#{PREFIX}_enderror_Das Programm hat zu lange gebraucht."
      client.puts msg
      thread.kill
    end
  end
end

#wait for incoming commands from the server
def get_commands(client, functions)
  # Thread to handel incoming messages

  loop do
    msg = client.gets.chomp
    puts "Incoming: #{msg}"

    msg = JSON.parse(msg)

    Thread.start do
      msg.each do |item|

        #Tests if the hash of the command is the same as the server says
        sechash = item['value'].chars.zip PEPPER.chars
        sechash = sechash.join
        sechash = Digest::SHA256.hexdigest(sechash)
        unless item['hash'] == sechash
          $stderr.puts 'The hash is not correct.'
          return
        end

        command = JSON.parse(item['value'])
        search_and_execute_function(functions, command.keys[0], command.values[0])
      end
    end
  end
end

# handle the commands in the queue.
# start next one, when the previous is finished
def handle_queue_functions(queue)
  Thread.start do
    loop do
      queue.pop.call
    end
  end
end

# call the function with the hash given
def search_and_execute_function(functions, name, hash)
  functions[name.to_sym].call(hash)
end

#send a response to the execution
def response(hash, shared)
  if shared[:stdin]
    shared[:stdin].puts hash['value']
  end
end

#send an exit to the server
def exit_command(hash, client, shared)
  #puts 'exit'
  if hash['successful']
    end_msg = "\n#{PREFIX}_end"
  else
    end_msg = "\n#{PREFIX}_enderror_#{hash['message']}"
  end

  if PERFORMANCE_TEST
    exit_time = Time.now
    start_time = shared[:start_time]

    diff = exit_time - start_time
    client.puts timing = "\n#{PREFIX}_timings_#{diff}"
    puts timing
  end

  puts end_msg
  client.puts end_msg
end

# write a file to the filesystem
def write_file(hash, dir)
  open("#{dir}/#{hash['filename']}", 'w+') do |file|
    File.write file, hash['content']
    File.chmod(hash['permissions'], file)
  end
end

# execute a given command
def execute(hash, client, dir, shared)
  command = hash['command'].gsub('$LIB$', Dir.pwd + '/lib').gsub('$PATH$', dir) #replace $LIB$ and $PATH$

  changeuser = 'sudo -u sailor ' # user to execute the command in a secure vm (no write permission and no internet connection)
  if DEVELOPMENT or hash['permissions'] == 'read-write'
    changeuser = ''
  end

  command = "cd #{dir} && " + changeuser + command
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

# handle the stdout-stream
# add a prefix to the output if tag is a string
def handle_stdout(client, stdout, tag, shared)
  counter = 0
  loop do
    if stdout.eof?

      #print errormessages at last
      if shared[:err]
        shared[:err].each_line do |line|
          #puts line
          client.puts line
        end
      end

      break
    elsif counter > MAX_OPS
      #tell the client that the execution has finished with errors
      msg = "#{PREFIX}_enderror_Die maximale Anzahl an Operationen wurde erreicht."
      client.puts msg
      break
    end
    counter += 1
    line = stdout.readline

    unless line.chomp.empty?
      unless tag == false || line.start_with?(PREFIX)
        line = "#{PREFIX}_print_#{tag}_#{line}"
      end
      #puts line
      client.puts line
    end
  end
end

# handle the stderr-stream
# add a prefix to the output, if tag is a stirng
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
        $stderr.puts "The IP '#{client.peeraddr[3]}' is not in the whitelist."
        return
      end
    end

    shared = {:start_time => Time.now} #share data over several methods
    queue = Queue.new #queue to handle commands one after another

    temp = '/codetemp'
    if DEVELOPMENT
      temp = '/tmp'
    end

    dir = "#{temp}/session_#{Time.now.to_i}_#{rand(1000000)}"

    thr = Thread.start {
      initialize_timeout(client)
      thread = Thread.current

      Dir.mkdir(dir, 0755)

      # functions that are supported by the vm
      functions = {:response => lambda { |hash| response(hash, shared) }, #execute immediate
                   :stop => lambda { |_| thread.kill },
                   :write_file => lambda { |hash| queue.push(lambda { write_file(hash, dir) }) }, #add to queue
                   :execute => lambda { |hash| queue.push(lambda { execute(hash, client, dir, shared) }) },
                   :exit => lambda { |hash| queue.push(lambda { exit_command(hash, client, shared) }) }}

      handle_queue_functions(queue)
      get_commands(client, functions)
    }

    thr.join #wait for execution to finish or stopped by timeout

    FileUtils.rm_r dir #clean up

  end
}.join
