#!/bin/bash

#curl -d @stats.xml http://admin:1234@192.168.1.88:10000/smartplug.cgi -o -
curl -d @xml/$1 http://admin:1234@192.168.1.88:10000/smartplug.cgi -o -
