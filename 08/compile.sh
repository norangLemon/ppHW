#!/usr/bin/env bash

for keyword in `cat forbidden.txt`; do
    grep -r ${keyword} submission/
    rc=$?; if [[ $rc != 1 ]]; then echo "Do not use following keyword: ${keyword}" ; exit 1; fi
done

rm -rf classes/
mkdir classes/
scalac -classpath classes/ -d classes/ Data.scala
scalac -classpath classes/ -d classes/ submission/Main.scala 
