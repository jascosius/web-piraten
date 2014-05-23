require 'socket'
require 'tmpdir'
class VirtualMachine
  def initialize(port, ip)
    @server = TCPServer.open(ip, port)
    run
  end

  def run
    loop {
      Thread.start(@server.accept) do |client|
        code = ''
        loop do
          msg = client.gets
          puts msg
          if msg.include?('CkyUHZVL3q_EOF')
            puts "Fertig"
            break
          end
          code += msg
        end

        puts 'Code erhalten'

        # create temporally file for execution of ruby code
        Dir.mktmpdir('session_') do |dir|
          # use the directory...
          open("#{dir}/code.rb", 'w+') do |file| #TODO: Create file as specific linux user

            puts code

            File.chmod 0777, file
            File.write file, code

            IO.popen("ruby #{File.path(file)}", 'r+') do |pipe|
              pipe.sync = true
              until pipe.eof?
                line = pipe.readline
                client.puts line
                if line.include?('CkyUHZVL3q_?')
                  msg = client.gets
                  pipe.write msg
                end
              end
              client.puts 'CkyUHZVL3q_end'
            end
          end
        end
      end
    }.join
  end
end

VirtualMachine.new(12340, "localhost")