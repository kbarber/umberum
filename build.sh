#!/bin/bash

# Now compile erlang source
function my_mkdir() {
    if [ ! -d $1 ]
    then mkdir -p $1
    fi
}

my_mkdir ebin/organic/logger

erl -make
