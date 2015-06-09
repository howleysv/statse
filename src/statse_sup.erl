-module( statse_sup ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( supervisor ).

%% Public interface
-export( [ start_link/0 ] ).

%% supervisor behaviour
-export( [ init/1 ] ).

-define( CHILD( M, F, A ), { M, { M, F, A }, permanent, 10000, worker, [ M ] } ).

start_link() ->
	supervisor:start_link( ?MODULE, [] ).

init( _ ) ->
	MaxRestart 	= 3,
	MaxTime 	= 60,
	ChildSpecs 	= [	?CHILD( statse_worker, start_link, [] ),
				?CHILD( statse_gauge_monitor, start_link, [] ) ],
	{ ok, { { one_for_one, MaxRestart, MaxTime }, ChildSpecs } }.
