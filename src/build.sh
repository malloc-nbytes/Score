#!/bin/bash

set -xe

snap run dmd *.d -of=scr -g
