#!/bin/bash
rsync -avzr --delete -e "ssh -p 12342" /var/www/webpiraten/current/vm/vm/ root@chevalblanc.informatik.uni-kiel.de:/home/captain/vm
ssh -p 12342 root@chevalblanc.informatik.uni-kiel.de 'chown -R root:root /home/captain/vm/lib; chmod -R 755 /home/captain/vm/lib; chown root:captain /home/captain/vm/vm.rb; chmod 740 /home/captain/vm/vm.rb; shutdown -h now'
exit 0