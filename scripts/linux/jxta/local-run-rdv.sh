#!/bin/bash

./run-rendezvous.sh 0 tcp://127.0.0.1:23701 tcp://127.0.0.1:24701 &
./run-rendezvous.sh 1 tcp://127.0.0.1:23701 tcp://127.0.0.1:24701 &
