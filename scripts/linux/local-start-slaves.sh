#!/bin/bash

./start-slaves.sh 4 tcp://127.0.0.1:23701 &
./start-slaves.sh 4 tcp://127.0.0.1:24701 &
