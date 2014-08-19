# -*- encoding : utf-8 -*-
require 'socket'
require 'open3'
require 'fileutils'
require 'json'
require 'thread'

PREFIX = 'CkyUHZVL3q' #have to be the same as in the socket_controller
TIMEOUT = 40 #have to be the same as in the socket_controller
MAX_OPS = 10000 #the maximal counter of ops to execute
PORT = 12340 #have to be the same as in the socket_controller

if ARGV[0] == 'production'
  puts 'Starting VM in production-mode.'
  DEVELOPMENT = false
else
  puts 'Starting VM in development-mode.'
  DEVELOPMENT = true
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
  shared = {}

  loop do
    msg = client.gets.chomp
    puts "Erhalten: #{msg}"

    msg = JSON.parse(msg)

    Thread.start do
      msg.each do |item|
        search_and_execute_function(functions, item.keys[0], item.values[0], shared)
      end
    end
  end
end


def search_and_execute_function(functions, name, hash, shared)
  functions.each do |key, value|
    if key.to_s == name
      value.call(hash, shared)
      break
    end
  end
end

def response(msg, shared)
  if shared[:stdin]
    shared[:stdin].puts msg
  end
end

def exit(hash, client)
  if hash['succsessful']
    puts msg = "\n#{PREFIX}_end"
    client.puts msg
  else
    puts msg = "\n#{PREFIX}_enderror_#{hash['message']}"
    client.puts msg
  end
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

  #:execute => {:command => nil, :stdout_prefix => false, :stderr_prefix => 'error', }}

  #Open3.popen2("(#{changeuser} #{execute.gsub('$PATH$', dir)} 3>&1 1>&2 2>&3 | sed --unbuffered s/^/#{PREFIX}_stderr_/ ) 2>&1") do |stdin, stdout|
  #["run_daemon", "-f", "some.conf", "--verbose", :err => [:child, :out]]
  puts command = "cd #{dir} && " + changeuser + command
  Open3.popen3(command) do |stdin, stdout, stderr|
    stdout.sync = true
    stdin.sync = true
    shared[:stdin] = stdin

    Thread.start do
      handle_stderr(client, stderr, hash['stderr'], shared)
    end
    handle_stdout(client, stdout, hash['stdout'], shared)
  end
end

def handle_stdout(client, stdout, tag, shared)
  counter = 0
  loop do
    if stdout.eof?

      #workarrount to print errormessages at last
      if shared[:err]
        shared[:err].each_line do |line|
          puts line
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

def handle_stderr(client, stderr, tag, shared)
  loop do
    line = stderr.readline

    unless line.chomp.empty?
      if tag
        #workarrout to print errormessages at last
        shared[:err] ||= ''
        shared[:err] = shared[:err] + "\n#{PREFIX}_print_#{tag}_#{line}"
        #puts line = "#{PREFIX}_print_#{tag}_#{line}" #to do here without the workarround
      end
    end
  end
end


server = TCPServer.new PORT
loop {
  Thread.start(server.accept) do |client| #spawn new process for a new client

    temp = '/codetemp'
    if DEVELOPMENT
      temp = '/tmp'
    end

    dir = "#{temp}/session_#{Time.now.nsec}_#{rand(1000000)}"

    thr = Thread.start {
      initialize_timeout(client)
      thread = Thread.current

      Dir.mkdir(dir, 0755)

      functions = {:response => lambda { |msg, shared| response(msg, shared) },
                   :write_file => lambda { |hash, _| write_file(hash, dir) },
                   :execute => lambda { |hash, shared| execute(hash, client, dir, shared) },
                   :exit => lambda { |hash, _| exit(hash, client) },
                   :stop => lambda { |_, _| puts 'stop'; thread.kill }}

      get_commands(client, functions)
    }

    thr.join

    FileUtils.rm_r dir

  end
}.join
