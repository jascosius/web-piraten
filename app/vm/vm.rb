require 'socket'
require 'tmpdir'
class VirtualMachine

  $prefix = 'CkyUHZVL3q_' #have to be the same as in the socket_controller
  @@timeout = 5 #have to be the same as in the socket_controller
  @@max_ops = 1000 #the maximal counter of ops to execute

  def initialize(port, ip)
    @server = TCPServer.open(ip, port)
    run
  end

  def run
    loop {
      Thread.start(@server.accept) do |client| #spawn new process for a new client

        Thread.start(Thread.current) do |thread|
          sleep(@@timeout)
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
          if msg.include?("#{$prefix}EOF")
            break
          end
          code += msg
        end

        # create temporally file for execution of ruby code
        Dir.mktmpdir('session_') do |dir|
          # use the directory...
          open("#{dir}/code.rb", 'w+') do |file| #TODO: Create file as specific linux user

            File.chmod 0777, file
            File.write file, code

            IO.popen("ruby #{File.path(file)}", 'r+') do |pipe|
              pipe.sync = true
              counter = 0
              loop do
                if pipe.eof?
                  #tell the client that the execution has finished successful
                  client.puts "#{$prefix}end"
                  break
                elsif counter > @@max_ops
                  #tell the client that the execution has finished with errors
                  client.puts "#{$prefix}end_error"
                  break
                end
                counter += 1
                line = pipe.readline
                puts line
                client.puts line

                #wait for an answer, when read a question
                if line.include?("#{$prefix}?")
                  msg = client.gets
                  pipe.write msg
                end
              end
            end
          end
        end
      end
    }.join
  end
end

VirtualMachine.new(12340, 'localhost')