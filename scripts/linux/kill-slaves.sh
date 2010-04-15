#!/bin/bash

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    ps -aef | grep 'com.jhurt.comm.Slave' | awk '{print $2}' | xargs kill -9
elif [[ "$unamestr" == 'Darwin' ]]; then
    ps | grep 'com.jhurt.comm.Slave' | awk '{print $1}' | xargs kill -9
else
    ps -aef | grep 'com.jhurt.comm.Slave' | awk '{print $2}' | xargs kill -9
fi
