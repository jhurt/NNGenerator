#!/bin/bash

ps | grep 'com.jhurt.p2p.SlaveR' | awk '{print $1}' | xargs kill -9
