-define(SYNCB, 16#47).
-define(TSLEN, 188).


-define(TS_b, <<?SYNCB:8, TEI:1, PUSI:1, TP:1, PID:13
		, SC:2, AD:2, CC:4
		, Payload:(?TSLEN-4)/binary>>).

-define(TS_t, {ts
	       , PID
	       , {TEI, PUSI, TP, SC, CC}
	       , AD
	       , Payload}).

-record(ts, {pid=0 :: integer()
	     , flags
	     , ad :: binary() | undefined
	     , payload :: binary() | undefined
	    }).

-record(ad, {flags
	     , pcr :: integer()
	     , opcr :: integer()
	     , splice_countdown :: integer
	    }).
