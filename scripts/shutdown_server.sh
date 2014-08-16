#!/bin/bash
cd /var/www/webpiraten/releases/$(cat ../../../last_version)
/home/captain/bin/bundle exec thin stop
exit 0
