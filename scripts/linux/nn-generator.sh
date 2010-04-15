#!/bin/bash

usage() { printf "Usage $0 <jms_broker_ip> <jms_broker_port>\n" >&2; }

if [ $# -lt 2 ]
then
    usage
    exit 1
fi

java -cp nn.jar:activemq-all-5.3.1.jar:piccolo.jar:clojure.jar com.jhurt.NNGenerator $@
