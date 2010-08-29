About
=====

A RELP protocol syslog receiver that writes out to a log file and MongoDB.

Installation
============

Prerequisites
-------------
* erlang R14A
  - kernel, stdlib, sasl
* emongo 0.0.6: http://bitbucket.org/japerk/emongo/
* mongodb 1.2.2

Development is done on Debian Squeeze, so the version of Erlang is quite new. This was a conscious decision due to the improvements in R14A and the assumption is that by the time this code is ready for prime time, most distributions would have caught up.

Failing that, we'll ship our own Erlang :-).

Compilation
-----------

Build the source code by running::

  make

To clean::

  make clean

Execution
---------

Run the application in the background::

  ./bin/ologctl start

Stop the backgrounded application with::

  ./bin/ologctl stop

You can run the application in the foreground with an active Erlang tty session.
This is perfect for debugging and development.::

  ./bin/ologctl startfg

From here you can do things like start the appmon process visualiser/manager::

  (ologd@localhost)1> appmon:start().

To exit tty just type Ctrl-C then hit 'q'.

Usage
-----

At this point the application is very much in heavy development and will 
be in various stages of 'working' depending on what revision you check out.

For now, I have a RELP listener working. If you have rsyslog, modify your 
configuration to include something like the following::

  $ModLoad omrelp
  *.*     :omrelp:127.0.0.1:2222;RSYSLOG_ForwardFormat

This will forward your logs to the listener on port 2222 in RELP format. If you
have mongodb running, check the 'test' database and you should see results 
appear.

Also tail /tmp/log to see the log file output. Again - this is all very basic 
for now - see RELEASE.rst list for things I want to work on. Most of the obvious
stuff is already there.

Copyright and License
=====================

For now its:

Copyright 2010 Kenneth E Barber

Until further notice, although the plan is to have it belong to an organisation
of some kind if I ever get around to assembling one.

The license for this code is undecided as yet. Assume nothing.


