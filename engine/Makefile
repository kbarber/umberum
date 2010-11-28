# Copyright 2010 Bob.sh Limited

all: 
	@mkdir -p ebin/umberum/logger
	@mkdir -p ebin/umberum/logger/syslog_3164
	@mkdir -p ebin/umberum/logger/route
	@mkdir -p ebin/umberum/logger/file
	@mkdir -p ebin/umberum/logger/mongodb
	@mkdir -p ebin/umberum/logger/tokenizer
	@mkdir -p ebin/umberum/event
	@mkdir -p ebin/umberum/event/proc
	@mkdir -p ebin/umberum/input
	@mkdir -p ebin/umberum/input/relp
	@erl -make

	@erlc 	-pz ../../emongo/ebin \
		-Iebin/ \
		-o ebin \
		ebin/umberum-1.rel 

clean: 
	rm -f ebin/*.beam
	rm -f ebin/*.boot
	rm -f ebin/*.script
	rm -fr ebin/umberum
	rm -fr doc/api
	rm -f erl_crash*

test: all
	@mkdir -p test_reports
	@erl	-pa ebin \
		-smp auto \
		-noshell \
		-noinput \
		-s umberum_alltests \
		start

docs:
	@mkdir -p doc/api/
	@erl	-noshell \
		-run edoc_run \
		application "umberum" "src" '[{dir, "doc/api"},{overview,"doc/overview.edoc"}]' \
		-s init stop

