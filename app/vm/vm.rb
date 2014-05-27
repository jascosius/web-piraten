# -*- encoding : utf-8 -*-
require 'socket'
require 'tmpdir'

PREFIX = 'CkyUHZVL3q_' #have to be the same as in the socket_controller
TIMEOUT = 5 #have to be the same as in the socket_controller
MAX_OPS = 1000 #the maximal counter of ops to execute
PORT = 12340

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
      open("#{dir}/code.rb", 'w+') do |file| #TODO: Create file as specific linux user

        #File.chmod(777, file)
        File.write file, code

        IO.popen("ruby #{File.path(file)}", 'r+') do |pipe|
          pipe.sync = true
          counter = 0
          loop do
            if pipe.eof?
              #tell the client that the execution has finished successful
              client.puts "#{PREFIX}end"
              break
            elsif counter > MAX_OPS
              #tell the client that the execution has finished with errors
              client.puts "#{PREFIX}end_error"
              puts 'max_ops reached'
              break
            end
            counter += 1
            line = pipe.readline
            puts line
            client.puts line

            #wait for an answer, when read a question
            if line.include?("#{PREFIX}?")
              msg = client.gets
              pipe.write msg
            end
          end
        end
      end
    end
  end
}.join
