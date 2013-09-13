-module( statse ).
-author( "Shane Howley <howleysv@gmail.com>" ).

%% Public interface
-export( [ increment/1, increment/2, decrement/1, decrement/2, count/2, count/3, timing/2, gauge/2, gauge_change/2 ] ).

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
timing( Stat, Timestamp = { _, _, _ } ) ->
	Millis = timer:now_diff( erlang:now(), Timestamp ) / 1000,
	timing( Stat, Millis );

timing( Stat, Millis ) ->
	dispatch( { timer, Stat, Millis } ).

-spec gauge( stat_key(), integer() ) -> ok.
gauge( Stat, Value ) ->
	dispatch( { gauge, Stat, Value } ).

-spec gauge_change( stat_key(), integer() ) -> ok.
gauge_change( Stat, Delta ) ->
	dispatch( { gauge_change, Stat, Delta } ).

-spec dispatch( tuple() ) -> ok.
dispatch( Message ) ->
	statse_worker:send_stat( statse_worker, Message ).
