#!/bin/bash

ssh compute-0-0 ./start-slaves.sh 2 &
ssh compute-0-1 ./start-slaves.sh 2 &
ssh compute-0-2 ./start-slaves.sh 2 &
ssh compute-0-3 ./start-slaves.sh 2 &
ssh compute-0-4 ./start-slaves.sh 2 &
ssh compute-0-5 ./start-slaves.sh 2 &
ssh compute-0-6 ./start-slaves.sh 2 &
./start-slaves.sh 2 &
