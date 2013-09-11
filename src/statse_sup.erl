-module( statse_sup ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( supervisor ).

%% Public interface
-export( [ start_link/0 ] ).

%% supervisor behaviour
-export( [ init/1 ] ).

start_link() ->
	supervisor:start_link( ?MODULE, [] ).

init( _ ) ->
	{ ok, {	{ one_for_one, 3, 60 },
		[ {	statse_worker,
			{ statse_worker, start_link, [] },
			permanent,
			5000,
			worker,
			[ statse_worker ] }
		] } }.
