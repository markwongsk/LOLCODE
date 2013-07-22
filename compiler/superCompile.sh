#! /bin/bash
./bin/l1c $1.l1
gcc $1.s ../runtime/l1rt.c
