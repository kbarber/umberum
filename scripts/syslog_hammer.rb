#!/usr/bin/env ruby

# == Synopsis
# 
# This script is used for load testing a syslog server.
#
# == Usage
#
# syslog_hammer.rb [option]
#
# Options:
#
# -h, --help:
#    show help
#
# -n x, --repeat x:
#    repeat x times. Defaults to 1.
#
require 'syslog'
require 'getoptlong'
require 'rdoc/usage'

opts = GetoptLong.new(
 ['--help', '-h', GetoptLong::NO_ARGUMENT ],
 ['--repeat', '-n',GetoptLong::REQUIRED_ARGUMENT ]
)

reps = 1
opts.each do |opt,arg|
  case opt
    when '--help'
      RDoc::usage
    when '--repeat'
      reps = arg.to_i
  end
end

Syslog.open('loadtest', facility = Syslog::LOG_USER)

(1..reps).each do |n|
  Syslog.crit("test: #{n}")
end
