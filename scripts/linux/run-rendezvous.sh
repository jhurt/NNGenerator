#!/bin/bash

usage() { printf "Usage $0 <index> <all_rendezvous_uris>+\n" >&2; }

if [ $# -lt 2 ]
then
    usage
    exit 1
fi

java -cp nn.jar:bcprov-jdk14.jar:javax.servlet.jar:jxta.jar:org.mortbay.jetty.jar:clojure.jar com.jhurt.p2p.Rendezvous $@
