#!/bin/bash

# Now compile erlang source
function my_mkdir() {
    if [ ! -d $1 ]
    then mkdir -p $1
    fi
}

my_mkdir ebin/organic/logger/relp

erl -make

cd ebin/
erlc -I. organic.logger-1.rel

#erl -s systools make_script organic.logger-1 -s erlang halt
