#!/bin/bash

for ((i=9;i<=24;i++));
do ./run-rendezvous.sh tcp://70.180.196.124:${i}701 &
done;
