#!/bin/bash

ANTLR_PATH="/home/iosif/devel/antlr4/antlr-4.7.1-complete.jar" 

java -Xmx500M -cp $ANTLR_PATH:$CLASSPATH org.antlr.v4.Tool C.g4; 
javac -cp $ANTLR_PATH:$CLASSPATH CMain.java
