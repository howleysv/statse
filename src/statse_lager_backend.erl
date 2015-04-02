-module( statse_lager_backend ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( gen_event ).

%% gen_event behaviour
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

-record( state, { level :: { mask, integer() } } ).

-spec init( lager:log_level() | string() ) -> { ok, #state{} }.
init( Level ) -> { ok, #state{ level = lager_util:config_to_mask( Level ) } }.

-spec handle_event( { log, lager_msg:lager_msg() }, #state{} ) -> { ok, #state{} }.
handle_event( { log, Message }, #state{ level = Level } = State ) ->
	case lager_util:is_loggable( Message, Level, ?MODULE ) of
		true ->	statse:increment( [ <<"lager.message.">>, atom_to_binary( lager_msg:severity( Message ), utf8 ) ] );
		_ -> 	ok
	end,
	{ ok, State };

handle_event( _Message, State ) ->
	{ ok, State }.

-spec handle_call( get_log_level | { set_log_level, lager:log_level() | string() }, #state{} ) -> { ok, term(), #state{} }.
handle_call( get_log_level, #state{ level = Level } = State ) ->
	{ ok, Level, State };

handle_call( { set_log_level, NewLevel }, #state{} = State ) ->
	try lager_util:config_to_mask( NewLevel ) of
		Level ->	{ ok, ok, State#state{ level = Level } }
	catch
		_:_ ->		{ ok, { error, bad_log_level }, State }
	end;

handle_call( _Request, State ) ->
	{ ok, ok, State }.

%% Ununsed callbacks
handle_info( _Message, State ) -> 		{ ok, State }.
terminate( _Reason, _State ) -> 		ok.
code_change( _OldVersion, State, _Extra ) -> 	{ ok, State }.
