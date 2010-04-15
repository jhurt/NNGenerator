#!/bin/bash

for ((i=1;i<=16;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:9701 &
done;

for ((i=17;i<=32;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:10701 &
done;

for ((i=33;i<=48;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:11701 &
done;

for ((i=49;i<=64;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:12701 &
done;

for ((i=65;i<=80;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:13701 &
done;

for ((i=71;i<=96;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:14701 &
done;

for ((i=97;i<=112;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:15701 &
done;

for ((i=113;i<=128;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:16701 &
done;

for ((i=129;i<=144;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:17701 &
done;

for ((i=145;i<=160;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:18701 &
done;

for ((i=161;i<=176;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:19701 &
done;

for ((i=177;i<=192;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:20701 &
done;

for ((i=193;i<=208;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:21701 &
done;

for ((i=209;i<=224;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:22701 &
done;

for ((i=225;i<=240;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:23701 &
done;

for ((i=241;i<=256;i++));
do ssh wulfdata-${i} ./start-slaves.sh 1 tcp://70.180.196.124:24701 &
done;
