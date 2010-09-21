#!/bin/bash

usage() { printf "Usage $0 <number_of_slaves> <jms_broker_ip> <jms_broker_port>\n" >&2; }

if [ $# -lt 3 ]
then
    usage
    exit 1
fi

for (( i=1;i<=$1;i+=1 )) ;
do java -XX:+AggressiveOpts -XX:+UseFastAccessorMethods -XX:+UseTLAB -XX:+UnlockExperimentalVMOptions -XX:+UseG1GC -cp nn.jar:activemq-all-5.3.1.jar:piccolo.jar:clojure.jar com.jhurt.comm.Slave slave$i $2 $3 &
done;
