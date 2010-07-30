#!/bin/bash

./clean.sh
./build.sh

cd ebin
erl -boot organic.logger-1
