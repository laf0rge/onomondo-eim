#!/bin/bash

if [[ $# == 2 ]]; then
    sleep 5
    while true; do
	./restop.py -l -f $1 -r $2
	sleep 5
    done
else
    echo "Usage: $0 facility resource-id"
fi



