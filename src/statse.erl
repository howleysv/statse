-module( statse ).
-author( "Shane Howley <howleysv@gmail.com>" ).

%% Public interface
-export( [ increment/1, increment/2, decrement/1, decrement/2, count/2, count/3, timing/2, timing/3, gauge/2, gauge_change/2, set/2 ] ).

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

-spec count( stat_key(), number() ) -> ok.
count( Stat, Count ) ->
	count( Stat, Count, 1.0 ).

-spec count( stat_key(), number(), float() ) -> ok.
count( Stat, Count, SampleRate ) when SampleRate >= 1.0 ->
	dispatch( { counter, Stat, Count } );

count( Stat, Count, SampleRate ) ->
	should_send( SampleRate ) andalso dispatch( { counter, Stat, Count, SampleRate } ),
	ok.

-spec timing( stat_key(), number() | erlang:timestamp() ) -> ok.
timing( Stat, Time ) ->
	timing( Stat, Time, 1.0 ).

-spec timing( stat_key(), number() | erlang:timestamp(), float() ) -> ok.
timing( Stat, Timestamp = { _, _, _ }, SampleRate ) ->
	Millis = timer:now_diff( os:timestamp(), Timestamp ) div 1000,
	timing( Stat, Millis, SampleRate );

timing( Stat, Millis, SampleRate ) when SampleRate >= 1.0 ->
	dispatch( { timer, Stat, Millis } );

timing( Stat, Millis, SampleRate ) ->
	should_send( SampleRate ) andalso dispatch( { timer, Stat, Millis, SampleRate } ),
	ok.

-spec gauge( stat_key(), number() ) -> ok.
gauge( Stat, Value ) ->
	dispatch( { gauge, Stat, Value } ).

-spec gauge_change( stat_key(), number() ) -> ok.
gauge_change( Stat, Delta ) ->
	dispatch( { gauge_change, Stat, Delta } ).

-spec set( stat_key(), number() | iodata() ) -> ok.
set( Stat, Value ) ->
	dispatch( { set, Stat, Value } ).

-spec dispatch( tuple() ) -> ok.
dispatch( Message ) ->
	statse_worker:send_stat( statse_worker, Message ).

-spec should_send( float() ) -> boolean().
should_send( SampleRate ) ->
	case get( random_seed ) of
		undefined ->	random:seed( os:timestamp() );
		_ ->		ok
	end,
	random:uniform() =< SampleRate.


-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( TEST_PORT, 53934 ).
-define( TEST_PREFIX, "statse.test" ).

-define( UDP_TEST( F ), { setup, fun setup/0, fun teardown/1, F } ).

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

udp_test_() ->
	[
		{ "Increment counter format",				?UDP_TEST( fun test_increment/1 ) },
		{ "Decrement counter format",				?UDP_TEST( fun test_decrement/1 ) },
		{ "Counter format", 					?UDP_TEST( fun test_count/1 ) },
		{ "Counter with sampling rate format", 			?UDP_TEST( fun test_count_sample/1 ) },
		{ "Timing format", 					?UDP_TEST( fun test_timing_ms/1 ) },
		{ "Timing with erlang timestamp start time format",	?UDP_TEST( fun test_timing_now/1 ) },
		{ "Timing with a sampling rate format", 		?UDP_TEST( fun test_timing_sample/1 ) },
		{ "Positive gauge format", 				?UDP_TEST( fun test_gauge/1 ) },
		{ "Positive gauge change format", 			?UDP_TEST( fun test_gauge_change/1 ) },
		{ "Negative gauge format", 				?UDP_TEST( fun test_gauge_negative/1 ) },
		{ "Set format", 					?UDP_TEST( fun test_set/1 ) },
		{ "Float value encoding format", 			?UDP_TEST( fun test_float/1 ) }
	].

gauge_monitor_test_() ->
	[ { 	"Test repeating gauge stats send by gauge monitor",
		{
			setup,
			fun() -> { setup(), element( 2, statse_gauge_monitor:start_link() ) } end,
			fun( { Port, Pid } ) -> exit( Pid, normal ), teardown( Port ) end,
			fun test_gauge_monitor/1
		}
	} ].

test_increment( Port ) ->
	Stat = "inc",
	increment( Stat ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1|c", Packet ) ].

test_decrement( Port ) ->
	Stat = "dec",
	decrement( Stat ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":-1|c", Packet ) ].

test_count( Port ) ->
	Stat = "count",
	count( Stat, 1234 ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|c", Packet ) ].

test_count_sample( Port ) ->
	Stat = "count.sampled",
	dispatch( { counter, Stat, 1234, 0.25 } ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|c|@0.25", Packet ) ].

test_timing_ms( Port ) ->
	Stat = "timing",
	timing( Stat, 1234 ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|ms", Packet ) ].

test_timing_now( Port ) ->
	Stat = "timing.now",
	{ Mega, Sec, Micro } = os:timestamp(),
	StartTime = { Mega - 1, Sec, Micro },
	timing( Stat, StartTime ),
	Packet = get_packet( Port ),
	{ ok, Regex } = re:compile( re:replace( ?TEST_PREFIX ++ "." ++ Stat, "\\.", "\\\\\\.", [ global, { return, list } ] ) ++ ":[0-9]+\\|ms" ),
	[ ?_assertEqual( match, re:run( Packet, Regex, [ { capture, none } ] ) ) ].

test_timing_sample( Port ) ->
	Stat = "timing.sampled",
	dispatch( { timer, Stat, 1234, 0.25 } ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|ms|@0.25", Packet ) ].

test_gauge( Port ) ->
	Stat = "gauge",
	gauge( Stat, 1234 ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|g", Packet ) ].

test_gauge_change( Port ) ->
	Stat = "gauge.change",
	gauge_change( Stat, 1234 ),
	Packet = get_packet( Port ),
	[ ?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":+1234|g", Packet ) ].

test_gauge_negative( Port ) ->
	Stat = "gauge.neg",
	gauge( Stat, -1234 ),
	Packet1 = get_packet( Port ),
	Packet2 = get_packet( Port ),
	[
	 	?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":0|g", Packet1 ),
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":-1234|g", Packet2 )
	].

test_set( Port ) ->
	Stat = "count",
	set( Stat, 1234 ),
	Packet1 = get_packet( Port ),
	set( Stat, "user5678" ),
	Packet2 = get_packet( Port ),
	[
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|s", Packet1 ),
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":user5678|s", Packet2 )
	].

test_float( Port ) ->
	Stat = "float",
	count( Stat, 1234.5678 ),
	Packet1 = get_packet( Port ),
	count( Stat, 0.0000000001 ),
	Packet2 = get_packet( Port ),
	count( Stat, -1234.5678 ),
	Packet3 = get_packet( Port ),
	count( Stat, 1.23456e78 ),
	Packet4 = get_packet( Port ),
	[
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234.5678|c", Packet1 ),
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1.0e-10|c", Packet2 ),
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":-1234.5678|c", Packet3 ),
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1.23456e78|c", Packet4 )
	].

test_gauge_monitor( { Port, _Pid } ) ->
	Stat = "gauge.mon",
	statse_gauge_monitor:start_monitor( Stat, fun() -> { ok, 1234 } end, 7 ),
	Packet1 = get_packet( Port ),
	Packet2 = get_packet( Port ),
	[
		?_assertEqual( ?TEST_PREFIX ++ "." ++ Stat ++ ":1234|g", Packet1 ),
		?_assertEqual( Packet1, Packet2 )
	].

-endif.
