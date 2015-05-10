#!/bin/bash
#get the last version of the server
#shut the server down
cd ~/webpiraten/releases/$(cat ~/webpiraten/last_version)
bundle exec thin stop
exit 0
