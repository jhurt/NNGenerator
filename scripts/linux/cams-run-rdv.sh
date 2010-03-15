#!/bin/bash

for ((i=9701;i<=9716;i++));
do ./run-rendezvous.sh tcp://70.180.196.124:${i} &
done;
