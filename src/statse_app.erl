-module( statse_app ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( application ).

%% application behaviour
-export( [ start/2, stop/1 ] ).

%% Start the application
start( _StartType, _StartArgs ) ->
	statse_sup:start_link().

%% Stop the application
stop( _State ) ->
	ok.
