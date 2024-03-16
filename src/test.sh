#!/bin/bash

./clean.sh
./build.sh
./scr ./test.scr
./test.scr.out
