#!/bin/bash
#get the last version of the server
#shut the server down
cd /var/www/webpiraten/releases/$(cat ../../../last_version)
/home/captain/bin/bundle exec thin stop
exit 0
