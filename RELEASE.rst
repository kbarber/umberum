Release Notes
=============

This document covers release specific notes. Here I'll try to document each 
release and the changes that were introduced.

Always remember: the only real authority for change is the code.

Future
------

Some of the todo's are littered throughout the app in source. Just
search for TODO.

Here I'll list some future todo's that I plan on working on. In the future this
may move to a proper tracker.

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

* Log output for the application itself to a log file somewhere, especially
  now that we have background execution woking.

* Structure based on how extensible bits will be written: For now I'm creating a
  supervisor for each component regardless of its architectural responsibility. 
  In the future we may want to split things into: in, processing and out. Think:
  what bits are pluggable and what is the easiest way to modularise. We also need
  to be mindful of potentially running different process types on different boxes.

* Support for other syslog protocols other then just RELP. The Syslog Protocol 
  (latest): http://tools.ietf.org/search/rfc5424 seems to be the latest one and 
  supports TCP. This is going to give us the most coverage I believe. There is a
  UDP variant available but I don't think this is quite as useful due to lower
  reliability.

Version 1 - not yet released
---------

Here is a rough list of the current features for this release.

* Added ologctl for start/stop operations
* Made RELP receiver work
* Store decoded packets in mongodb
* Logging to a single file
* Created Emakefile used by emake to build Erlang specific bits
* Created Makefile for complete build purposes as emake is not holistic enough,
  and make is more familiar to the average punter
* organic.conf module and conf.hrl header provide macros and functions for 
  accessing basic global configuration. Currently this configuration is provided
  in sys.config, but may change. The abstraction from mechanism is provided for
  this in the ?CONF macro.
* Logging macros and functions.
