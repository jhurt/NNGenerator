#!/bin/bash

for ((i=1;i<=256;i++));
do ssh wulfdata-${i} killall -9 java &
done;
