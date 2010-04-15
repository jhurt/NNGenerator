#!/bin/bash

java -cp nn.jar:activemq-all-5.3.1.jar:piccolo.jar:clojure.jar com.jhurt.NNGenerator $@
