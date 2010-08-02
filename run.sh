#!/bin/bash

./clean.sh
./build.sh

cd ebin
erl +P 100000000 +A 1 +K true -boot organic.logger-1
