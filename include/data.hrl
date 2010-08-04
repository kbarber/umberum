%% This record represents a decoded RELP packet. Trailer and data length
%% are not provided here as they are superfluous once the packet is decoded
%% and validated.
%%
%% Currently this record is used by:
%% - con_fsm
%% - session_fsm
-record(relp, {
	txnr,		% Transaction number
	command,	% RELP command
	data		% Data payload
	}).

%% This record represents a standard BSD syslog packet once decoded.
%%
%% It is used by:
%% - decode_fsm
-record(syslog, {
	facility,
	severity,
	timestamp,
	hostname,
	tag,
	content
	}).
