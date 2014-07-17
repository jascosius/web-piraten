PATH_TO_VMSCRIPT= '/home/marius/VMs/qemu/control.sh'

if Rails.env.development?
  Thread.start do
    puts 'Started virtual machine for simulation (development only)'
    # blocking the thread
    Thread.new {system('ruby -C vm/vm vm.rb development')}
  end
else
  Thread.start do
    puts 'Started virtual machine for simulation (production)'
    # blocking the thread
    Thread.new {system('ruby -C vm/vm vm.rb production')}
  end
end