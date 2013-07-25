#!/bin/bash

STAMP="`date`"
TEST="tests1"

./driver.pl --suite $TEST > ../log/"stamp_$STAMP.log" 
