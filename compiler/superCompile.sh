#!/bin/bash
./bin/l1c $1.l1
gcc $1.s ../runtime/LOLCODE_runtime.c
