#!/bin/bash

CLOJURE_DIR=/Users/jleehurt/Dev/clojure-read-only
java -cp nn.jar:lib/jxta/bcprov-jdk14.jar:lib/jxta/javax.servlet.jar:lib/jxta/jxta.jar:lib/jxta/org.mortbay.jetty.jar:$CLOJURE_DIR/clojure.jar com.jhurt.p2p.Slave
