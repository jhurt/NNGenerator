#!/bin/bash

usage() { printf "Usage $0 <slave_name>\n" >&2; }

if [ $# -lt 1 ]
then
    usage
    exit 1
fi

java -cp nn.jar:activemq-all-5.3.1.jar:piccolo.jar:clojure.jar com.jhurt.comm.Slave $@
