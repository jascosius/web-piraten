require 'socket'
require 'tmpdir'
class VirtualMachine
  def initialize(port, ip)
    @server = TCPServer.open(ip, port)
    run
  end

  def run
    prefix = 'CkyUHZVL3q_' #have to be the same as in the socket_controller
    loop {
      Thread.start(@server.accept) do |client| #spawn new process for a new client

        #get programmcode from client
        code = ''
        loop do
          msg = client.gets
          puts msg
          if msg.include?("#{prefix}EOF")
            puts "Fertig"
            break
          end
          code += msg
        end

        puts code

        # create temporally file for execution of ruby code
        Dir.mktmpdir('session_') do |dir|
          # use the directory...
          open("#{dir}/code.rb", 'w+') do |file| #TODO: Create file as specific linux user

            File.chmod 0777, file
            File.write file, code

            IO.popen("ruby #{File.path(file)}", 'r+') do |pipe|
              pipe.sync = true
              until pipe.eof?
                line = pipe.readline
                client.puts line

                #wait for an answer, when read a question
                if line.include?("#{prefix}_?")
                  msg = client.gets
                  pipe.write msg
                end
              end

              #tell the client that the execution has finished
              client.puts "#{prefix}_end"
            end
          end
        end
      end
    }.join
  end
end

VirtualMachine.new(12340, "localhost")