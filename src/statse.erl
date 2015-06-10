-module( statse ).
-author( "Shane Howley <howleysv@gmail.com>" ).

%% Public interface
-export( [ increment/1, increment/2, decrement/1, decrement/2, count/2, count/3, timing/2, timing/3, gauge/2, gauge_change/2 ] ).

-export_type( [ stat_key/0 ] ).

-type stat_key() :: iodata().

-spec increment( stat_key() ) -> ok.
increment( Stat ) ->
	count( Stat, 1 ).

-spec increment( stat_key(), float() ) -> ok.
increment( Stat, SampleRate ) ->
	count( Stat, 1, SampleRate ).

-spec decrement( stat_key() ) -> ok.
decrement( Stat ) ->
	count( Stat, -1 ).

-spec decrement( stat_key(), float() ) -> ok.
decrement( Stat, SampleRate ) ->
	count( Stat, -1, SampleRate ).

-spec count( stat_key(), integer() ) -> ok.
count( Stat, Count ) ->
	count( Stat, Count, 1.0 ).

-spec count( stat_key(), integer(), float() ) -> ok.
count( Stat, Count, SampleRate ) when SampleRate >= 1.0 ->
	dispatch( { counter, Stat, Count } );

count( Stat, Count, SampleRate ) ->
	dispatch( { counter, Stat, Count, SampleRate } ).

-spec timing( stat_key(), integer() | erlang:timestamp() ) -> ok.
timing( Stat, Time ) ->
	timing( Stat, Time, 1.0 ).

-spec timing( stat_key(), integer() | erlang:timestamp(), float() ) -> ok.
timing( Stat, Timestamp = { _, _, _ }, SampleRate ) ->
	Millis = timer:now_diff( erlang:now(), Timestamp ) div 1000,
	timing( Stat, Millis, SampleRate );

timing( Stat, Millis, SampleRate ) when SampleRate >= 1.0 ->
	dispatch( { timer, Stat, Millis } );

timing( Stat, Millis, SampleRate ) ->
	dispatch( { timer, Stat, Millis, SampleRate } ).

-spec gauge( stat_key(), integer() ) -> ok.
gauge( Stat, Value ) ->
	dispatch( { gauge, Stat, Value } ).

-spec gauge_change( stat_key(), integer() ) -> ok.
gauge_change( Stat, Delta ) ->
	dispatch( { gauge_change, Stat, Delta } ).

-spec dispatch( tuple() ) -> ok.
dispatch( Message ) ->
	statse_worker:send_stat( statse_worker, Message ).


-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( TEST_PORT, 53934 ).
-define( TEST_PREFIX, "statse.test" ).

setup() ->
	{ ok, Port } = gen_udp:open( ?TEST_PORT, [ { active, false } ] ),
	{ ok, _ } = statse_worker:start_link( "localhost", ?TEST_PORT, ?TEST_PREFIX ),
	Port.

get_packet( Port ) ->
	{ ok, { _Address, _Port, Packet } } = gen_udp:recv( Port, 1000 ),
	Packet.

teardown( Port ) ->
	statse_worker:stop( statse_worker ),
	gen_udp:close( Port ).

increment_test() ->
	Port = setup(),
	Stat = "inc",
	increment( Stat ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1|c", Packet ),
	teardown( Port ).

decrement_test() ->
	Port = setup(),
	Stat = "dec",
	decrement( Stat ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":-1|c", Packet ),
	teardown( Port ).

count_test() ->
	Port = setup(),
	Stat = "count",
	count( Stat, 1234 ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|c", Packet ),
	teardown( Port ).

count_sample_test() ->
	Port = setup(),
	Stat = "count.sampled",
	dispatch( { counter, Stat, 1234, 0.25 } ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|c|@0.25", Packet ),
	teardown( Port ).

timing_ms_test() ->
	Port = setup(),
	Stat = "timing",
	timing( Stat, 1234 ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|ms", Packet ),
	teardown( Port ).

timing_now_test() ->
	Port = setup(),
	Stat = "timing.now",
	{ Mega, Sec, Micro } = os:timestamp(),
	StartTime = { Mega - 1, Sec, Micro },
	timing( Stat, StartTime ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1000000000|ms", Packet ),
	teardown( Port ).

timing_sample_test() ->
	Port = setup(),
	Stat = "timing.sampled",
	dispatch( { timer, Stat, 1234, 0.25 } ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|ms|@0.25", Packet ),
	teardown( Port ).

gauge_test() ->
	Port = setup(),
	Stat = "gauge",
	gauge( Stat, 1234 ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|g", Packet ),
	teardown( Port ).

gauge_change_test() ->
	Port = setup(),
	Stat = "gauge.change",
	gauge_change( Stat, 1234 ),
	Packet = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":+1234|g", Packet ),
	teardown( Port ).

gauge_negative_test() ->
	Port = setup(),
	Stat = "gauge.neg",
	gauge( Stat, -1234 ),
	Packet1 = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":0|g", Packet1 ),
	Packet2 = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":-1234|g", Packet2 ),
	teardown( Port ).

gauge_monitor_test() ->
	Port = setup(),
	{ ok, Pid } = statse_gauge_monitor:start_link(),
	Stat = "gauge.mon",
	statse_gauge_monitor:start_monitor( Stat, fun() -> { ok, 1234 } end, 7 ),
	Packet1 = get_packet( Port ),
	?assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|g", Packet1 ),
	timer:sleep( 10 ),
	Packet2 = get_packet( Port ),
	?assertEqual( Packet1, Packet2 ),
	exit( Pid, normal ),
	teardown( Port ).

-endif.
