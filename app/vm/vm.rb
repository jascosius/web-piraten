# -*- encoding : utf-8 -*-
require 'socket'
require 'tmpdir'
require 'open3'

PREFIX = 'CkyUHZVL3q' #have to be the same as in the socket_controller
TIMEOUT = 5 #have to be the same as in the socket_controller
MAX_OPS = 1000 #the maximal counter of ops to execute
PORT = 12340 #have to be the same as in the socket_controller

#thread to kill the execution after a while
def initialize_timeout(client)
  Thread.start(Thread.current) do |thread|
    sleep(TIMEOUT)
    if thread.alive?
      puts msg = "#{PREFIX}_enderror_Das Programm hat zu lange gebraucht."
      client.puts msg
      thread.kill
    end
  end
end

#execute the compile_command and stop the execution, when exit with errors
def compile(client, dir, compile, compile_error)
  exec = true
  if compile != ''
    #execute the compilecommand with the right path. add PREFIXstderr_compile to errors
    Open3.popen2("(#{compile.gsub('$PATH$', dir)} 3>&1 1>&2 2>&3 | sed --unbuffered s/^/#{PREFIX}_stderrcompile_/ ) 2>&1", 'r+') do |_, stdout|
      line = ''
      unless stdout.eof?
        puts line = stdout.readline
        client.puts line
      end
      if  compile_error != '' and line.include? compile_error #check if there is a compileerror
        puts msg = "#{PREFIX}_enderror_Beim Compilieren ist ein Fehler aufgetreten."
        client.puts msg
        exec = false
      end
    end
  end
  exec
end

#wait for incoming commands from the server
def get_commands(client, stdin)
  # Thread to handel incomming messages
  Thread.start(Thread.current) do |thread|
    while thread.alive? do
      msg = client.gets.chomp
      puts "Erhalten: #{msg}"
      array = msg.split('_')
      if array[0] == 'command'
        if array[1] == 'stop'
          thread.kill
          break
        end
      elsif array[0] == 'response'
        stdin.puts array[1..-1].join('_')
      else
        puts 'Unknown message: no command or response'
      end
    end
  end
end

#send commends from the execution to the server
def send_commands(client, stdout, execute_error)
  counter = 0
  loop do
    if stdout.eof?
      #tell the client that the execution has finished successful
      puts msg = "#{PREFIX}_end"
      client.puts msg
      break
    elsif counter > MAX_OPS
      #tell the client that the execution has finished with errors
      puts msg = "#{PREFIX}_enderror_Die maximale Anzahl an Operationen wurde erreicht."
      client.puts msg
      break
    end
    counter += 1
    line = stdout.readline
    if execute_error != '' and line.include? execute_error #check if the compiled file is found. Maybe not in case of a compileerror
      puts msg = "#{PREFIX}_enderror_Beim Compilieren ist ein Fehler aufgetreten."
      client.puts msg
      break
    end

    puts line
    client.puts line

  end
end

def execute(client, dir, execute, execute_error)
  #execute the executecommand with the right path. add PREFIXstderr to errors
  Open3.popen2("(#{execute.gsub('$PATH$', dir)} 3>&1 1>&2 2>&3 | sed --unbuffered s/^/#{PREFIX}_stderr_/ ) 2>&1") do |stdin, stdout|
    stdout.sync = true
    get_commands(client, stdin)
    send_commands(client, stdout, execute_error)
  end
end

server = TCPServer.new PORT
loop {
  Thread.start(server.accept) do |client| #spawn new process for a new client

    initialize_timeout(client)

    #get commands from client
    puts filename = client.gets.chomp #filename for the code
    puts compile = client.gets.chomp #command to compile the code
    puts execute = client.gets.chomp #command to execute the code
    puts compile_error = client.gets.chomp #string to find an compile error in the output of the compiler
    puts execute_error = client.gets.chomp #string to find an compile error in the output of the execution (can't find compiled file)

    #get programmcode from client
    code = ''
    loop do
      puts msg = client.gets
      if msg.include?("#{PREFIX}_EOF") #end of programmcode
        break
      end
      code += msg
    end

    # create temporally file for execution of ruby code
    Dir.mktmpdir('session_') do |dir|
      # use the directory...
      open("#{dir}/#{filename}", 'w+') do |file|
        File.write file, code

        exec = compile(client, dir, compile, compile_error)

        if exec
          execute(client, dir, execute, execute_error)
        end
      end
    end
  end
}.join
