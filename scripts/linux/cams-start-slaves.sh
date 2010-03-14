#!/bin/bash

for ((i=1;i<=256;i++));
do ssh wulfdata-${i} ./slaves.sh 1 &
done;
