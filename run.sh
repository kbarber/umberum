#!/bin/bash

./clean.sh
./build.sh

cd ebin
erl -pz ../../emongo/ebin +P 100000000 +A 1 +K true -boot organic.logger-1
