# -*- encoding : utf-8 -*-
require 'socket'
require 'tmpdir'

PREFIX = 'CkyUHZVL3q_' #have to be the same as in the socket_controller
TIMEOUT = 5 #have to be the same as in the socket_controller
MAX_OPS = 1000 #the maximal counter of ops to execute
PORT = 12340 #have to be the same as in the socket_controller

server = TCPServer.new PORT
loop {
  Thread.start(server.accept) do |client| #spawn new process for a new client

    Thread.start(Thread.current) do |thread|
      sleep(TIMEOUT)
      if thread.alive?
        puts 'kill'
        thread.kill
      end
    end

    #get commands from client
    filename = client.gets.chomp
    compile = client.gets.chomp
    execute = client.gets.chomp
    compile_error = client.gets.chomp
    execute_error = client.gets.chomp


    #get programmcode from client
    code = ''
    loop do
      msg = client.gets
      puts msg
      if msg.include?("#{PREFIX}EOF")
        break
      end
      code += msg
    end

    # create temporally file for execution of ruby code
    Dir.mktmpdir('session_') do |dir|
      # use the directory...
      open("#{dir}/#{filename}", 'w+') do |file| #TODO: Create file as specific linux user

        #File.chmod(777, file)
        File.write file, code

        exec = true
        if not compile == ''
          IO.popen("(#{compile.gsub('$PATH$', dir)} 3>&1 1>&2 2>&3 | sed s/^/#{PREFIX}stderr_compile/ ) 2>&1", 'r+') do |pipe|
            line = ''
            loop do
              if pipe.eof?
                puts "Letzte Zeile #{line}"
                if (not compile_error == '') and line.include? compile_error
                  client.puts "#{PREFIX}end_errorFehler beim Compilieren. (Beim Compilieren bemerkt)"
                  exec = false
                end
                break
              end
              line = pipe.readline
              client.puts line
            end
          end
        end

        if (exec)
          IO.popen("(#{execute.gsub('$PATH$', dir)} 3>&1 1>&2 2>&3 | sed s/^/#{PREFIX}stderr/ ) 2>&1", 'r+') do |pipe|
            pipe.sync = true
            counter = 0
            loop do
              if pipe.eof?
                #tell the client that the execution has finished successful
                client.puts "#{PREFIX}end"
                break
              elsif counter > MAX_OPS
                #tell the client that the execution has finished with errors
                client.puts "#{PREFIX}end_errorMaximale Anzahl der Operationen erreicht."
                puts 'max_ops reached'
                break
              end
              counter += 1
              line = pipe.readline
              puts line
              if (not execute_error == '') and line.include? execute_error
                client.puts "#{PREFIX}end_errorFehler beim Compilieren. (Beim Ausführen bemerkt)"
                break
              end

              client.puts line

              #wait for an answer, when read a question
              if line.include?("#{PREFIX}?")
                msg = client.gets
                puts msg
                if msg.include?("#{PREFIX}!_stop")
                  puts 'Ausführung beendet'
                  break
                else
                  pipe.write msg
                end
              end
            end
          end
        end
      end
    end
  end
}.join
