#!/bin/bash
set -xe
llc --opaque-pointers -o input.s input.ll
cc -o input input.s
