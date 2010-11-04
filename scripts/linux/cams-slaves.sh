#!/bin/bash

for ((i=1;i<=256;i++));
do ssh wulfdata-${i} ./start-slaves.sh 2 24.253.64.253 61616
done;