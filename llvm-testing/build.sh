#!/bin/bash
set -xe
llc -o input.s input.ll
cc -o input input.s
