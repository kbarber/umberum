# Copyright 2010 Bob.sh Limited

all: 
	@mkdir -p ebin/organic/logger
	@mkdir -p ebin/organic/logger/relp
	@mkdir -p ebin/organic/logger/syslog_3164
	@mkdir -p ebin/organic/logger/route
	@mkdir -p ebin/organic/logger/file
	@mkdir -p ebin/organic/logger/mongodb
	@mkdir -p ebin/organic/logger/tokenizer
	@mkdir -p ebin/organic/event
	@mkdir -p ebin/organic/event/proc
	@erl -make

	@erlc 	-pz ../emongo/ebin \
		-Iebin/ \
		-o ebin \
		ebin/organic-1.rel 

clean: 
	rm -f ebin/*.beam
	rm -f ebin/*.boot
	rm -f ebin/*.script
	rm -fr ebin/organic
	rm -fr doc/api
	rm -f erl_crash*

test:
	echo "Testing ..."
