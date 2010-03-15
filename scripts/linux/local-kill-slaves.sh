#!/bin/bash

ps | grep 'com.jhurt.p2p.Rendezvous' | awk '{print $1}' | xargs kill -9
