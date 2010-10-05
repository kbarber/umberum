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

Development is done on Debian Squeeze, so the version of Erlang is quite new. 
This was a conscious decision due to the improvements in R14A and the assumption 
is that by the time this code is ready for prime time, most distributions would 
have caught up.

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

Administration
--------------

You can run the application in the foreground with an active Erlang tty session.
This is perfect for debugging and development.::

  ./bin/ologctl startfg

From here you can do things like start the appmon process visualiser/manager::

  (ologd@localhost)1> appmon:start().

To exit tty just type Ctrl-C then hit 'q'.

Alternatively you can run the toolbar application while the logger is in the
background::

  ./bin/ologctl toolbar

Or you can run webtools::

  ./bin/ologctl webtools

Which can normally be accessed via HTTP on port 8888.

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

Copyright (C) 2010 Bob.sh

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
