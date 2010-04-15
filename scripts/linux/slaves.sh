#!/bin/bash

usage() { printf "Usage $0 <number_of_slaves>\n" >&2; }

if [ $# -lt 1 ]
then
    usage
    exit 1
fi

for (( i=1;i<=$1;i+=1 )) ;
do java -cp nn.jar:activemq-all-5.3.1.jar:piccolo.jar:clojure.jar com.jhurt.comm.Slave slave$i &
done;
