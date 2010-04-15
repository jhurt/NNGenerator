#!/bin/bash

usage() { printf "Usage $0 <rendezvous_uri>+\n" >&2; }

if [ $# -lt 1 ]
then
    usage
    exit 1
fi

java -cp nn.jar:bcprov-jdk14.jar:javax.servlet.jar:jxta.jar:org.mortbay.jetty.jar:piccolo.jar:clojure.jar com.jhurt.NNGenerator $@
