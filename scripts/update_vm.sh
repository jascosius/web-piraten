#!/bin/bash
#copy the vm-script to the vm
#set the permissions for the vm
#shutdown the vm
~/bin/control vm edit start
sleep 10
rsync -avzr --delete -e "ssh -p 12342" --rsync-path="sudo rsync" --exclude="vm.mine.conf.rb" ~/webpiraten/current/vm/vm/ navigator@chevalblanc.informatik.uni-kiel.de:/home/captain/vm
ssh -p 12342 navigator@chevalblanc.informatik.uni-kiel.de 'sudo chown -R root:root /home/captain/vm/lib; sudo chmod -R 755 /home/captain/vm/lib; sudo chown root:captain /home/captain/vm/*.rb; sudo chmod 740 /home/captain/vm/*.rb; sudo shutdown -h now'
sleep 10
~/bin/control vm use restart
exit 0