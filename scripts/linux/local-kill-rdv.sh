#!/bin/bash

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    ps -aef | grep 'com.jhurt.p2p.Rendezvous' | awk '{print $2}' | xargs kill -9
elif [[ "$unamestr" == 'Darwin' ]]; then
    ps | grep 'com.jhurt.p2p.Rendezvous' | awk '{print $1}' | xargs kill -9
else
    ps -aef | grep 'com.jhurt.p2p.Rendezvous' | awk '{print $2}' | xargs kill -9
fi
