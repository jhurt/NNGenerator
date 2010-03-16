#!/bin/bash

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'FreeBSD' ]]; then
   platform='freebsd'
fi

echo $unamestr


#ps | grep 'com.jhurt.p2p.SlaveR' | awk '{print $1}' | xargs kill -9

#ps -aef | grep com.jhurt.p2p.SlaveR | awk '{print $2}' | xargs kill -9
