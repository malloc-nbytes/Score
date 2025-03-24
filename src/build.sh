#!/bin/bash

set -xe

snap run dmd -g *.d -of=scr -g
./scr
