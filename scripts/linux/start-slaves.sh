#!/bin/bash

usage() { printf "Usage $0 <number_of_slaves> <rendezvous_uri>\n" >&2; }

if [ $# -lt 2 ]
then
    usage
    exit 1
fi

for (( i=1;i<=$1;i+=1 )) ;
do java -cp nn.jar:bcprov-jdk14.jar:javax.servlet.jar:jxta.jar:org.mortbay.jetty.jar:clojure.jar com.jhurt.p2p.SlaveR $2 &
done;
