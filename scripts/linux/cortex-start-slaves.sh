#!/bin/bash

ssh compute-0-0 ./slaves.sh 2 &
ssh compute-0-1 ./slaves.sh 2 &
ssh compute-0-2 ./slaves.sh 2 &
ssh compute-0-3 ./slaves.sh 2 &
ssh compute-0-4 ./slaves.sh 2 &
ssh compute-0-5 ./slaves.sh 2 &
ssh compute-0-6 ./slaves.sh 2 &
./slaves.sh 2 &
