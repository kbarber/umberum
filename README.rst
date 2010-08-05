About
=====

A syslog receiver.

Installation
============

Prerequisites:
* erlang R13B (kernel,stdlib,sasl)
* emongo 0.0.6: http://bitbucket.org/japerk/emongo/
* mongodb 1.2.2

Build::

  ./build.sh

Clean::

  ./clean

Run::

  ./run.sh


Todo
====

Most of the todo's are littered throughout the app in source. Just
search for TODO.

Here I'll list some global todo's. Until this code reaches a
reasonable stage of development this is good enough for now.

* Some FSM's should be servers, or pools: not bound to one inbound
  connection. This especially applies to UDP connections. I'm not sure
  of the cost of starting processes but it feel more 'right' to have
  a pool of workers ready to act that aren't bound to a single requester
  process. Examples are:
  - organic.logger.syslog_3164.decode_fsm
  - organic.logger.route.route_fsm.erl
  I think process groups (using pg2) is the way to go, using a supervisor
  to restart broken processes and keeping a constant number of pooled 
  processes. Of course, how does this pool scale under different load one asks ...

* Common place for configuration: I envision lots of configuration. Here is some
  that we could use now:
  - RELP listener port
  - connection timeout
  - output file name
  Configuration is a hairy beast - so I'm happy to wait before fixing this one. 
  I'd like more use cases to understand the best direction.

* Structure based on how extensible bits will be written: For now I'm creating a
  supervisor for each component regardless of its architectural responsibility. 
  In the future we may want to split things into: in, processing and out. Think:
  what bits are pluggable and what is the easiest way to modularise. We also need
  to be mindful of potentially running different process types on different boxes.
