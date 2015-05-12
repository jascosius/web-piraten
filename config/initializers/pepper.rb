#get a pepper to communicate with the vm
#the vm must have the same pepper
#IMPORTANT: The pepper is a secret

if Rails.env.development?
  PEPPER = ''
  puts 'The pepper is empty.'
else
  pepper = ''
  begin
    File.open(PEPPER_PATH_RAILS) do |file|
      pepper = file.read
    end
  rescue
    $stderr.puts "There is no pepper in '#{PEPPER_PATH_RAILS}'"
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

