-module( statse_gauge_monitor ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( gen_server ).

%% Public interface
-export( [ start_link/0, start_monitor/3, stop_monitor/1 ] ).

%% gen_server behavour
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export_type( [ stat_fun/0 ] ).

-type stat_fun()	:: fun( () -> number() | { ok, number() } ).

%% Monitored stat
-record( monitored, {
			key 		:: statse:stat_key(),
			stat_fun	:: stat_fun(),
			refresh		:: pos_integer(),
			timer 		:: reference()
		} ).

%% Internal State
-record( state, {
			monitored = []	:: [ #monitored{} ]	%% List of monitored stats
		} ).

-type call_type() 	:: _.
-type cast_type()	:: { start, statse:stat_key(), stat_fun(), pos_integer() }
			|  { stop, statse:stat_key() }.
-type info_type()	:: { timeout, reference(), statse:stat_key() }.
-type call_ret() 	:: { noreply, #state{} }.
-type cast_ret()	:: { noreply, #state{} }
			|  { stop, term(), #state{} }.

-spec start_link() -> { ok, pid() }.
start_link() ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

-spec start_monitor( statse:stat_key(), stat_fun(), pos_integer() ) -> ok | { error, term() }.
start_monitor( StatKey, StatFun, RefreshPeriod ) ->
	gen_server:cast( ?MODULE, { start, StatKey, StatFun, RefreshPeriod } ).

-spec stop_monitor( statse:stat_key() ) -> ok.
stop_monitor( StatKey ) ->
	gen_server:cast( ?MODULE, { stop, StatKey } ).

-spec init( _ ) -> { ok, #state{} }.
init( _ ) ->
	{ ok, #state{} }.

-spec handle_call( call_type(), { pid(), _ }, #state{} ) -> call_ret().
%% Unrecognised request
handle_call( _Request, _From, State ) ->
	error_logger:warning_msg( "Unrecognized call: ~p", [ _Request ] ),
	{ noreply, State }.

-spec handle_cast( cast_type(), #state{} ) -> cast_ret().
handle_cast( { start, StatKey, StatFun, RefreshPeriod }, #state{ monitored = Monitored } = State ) ->
	StatMonitor = #monitored{	key 		= StatKey,
					stat_fun 	= StatFun,
					refresh 	= RefreshPeriod },
	case log_stat( StatMonitor ) of
		ok ->
			NewMonitored = [ start_timer( StatMonitor ) | remove_monitor( StatKey, Monitored ) ],
			{ noreply, State#state{ monitored = NewMonitored } };
		{ error, Reason } ->
			error_logger:warning_msg( "Failed to add stat ~p ~p.", [ StatKey, Reason ] ),
			{ noreply, State }
	end;

handle_cast( { stop, StatKey }, #state{ monitored = Monitored } = State ) ->
	{ noreply, State#state{ monitored = remove_monitor( StatKey, Monitored ) } };

%% Unrecognised request
handle_cast( _Request, State ) ->
	error_logger:warning_msg( "Unrecognized cast: ~p", [ _Request ] ),
	{ noreply, State }.

-spec handle_info( info_type(), #state{} ) -> cast_ret().
handle_info( { timeout, Timer, StatKey }, #state{ monitored = Monitored } = State ) ->
	case lists:keytake( StatKey, #monitored.key, Monitored ) of
		{ value, #monitored{ timer = Timer } = StatMonitor, OtherMonitored } ->
			case log_stat( StatMonitor ) of
				ok ->
					{ noreply, State#state{ monitored = [ start_timer( StatMonitor ) | OtherMonitored ] } };
				{ error, Reason } ->
					error_logger:warning_msg( "Failed to log stat ~p ~p. Removing from stat monitor...", [ StatKey, Reason ] ),
					{ noreply, State#state{ monitored = OtherMonitored } }
			end;
		_ ->
			{ noreply, State }
	end;

%% Unrecognised message
handle_info( _Message, State ) ->
	error_logger:warning_msg( "Unrecognized message: ~p", [ _Message ] ),
	{ noreply, State }.

-spec terminate( term(), #state{} ) -> _.
terminate( _Reason, #state{} ) ->
	ok.

-spec code_change( term(), #state{}, term() ) -> { ok, #state{} }.
code_change( _OldVersion, State, _Extra ) ->
	{ ok, State }.

-spec log_stat( #monitored{} ) -> ok | { error, term() }.
log_stat( #monitored{ key = StatKey, stat_fun = Fun } ) ->
	try
		Stat = case Fun() of
			S when is_number( S ) ->		S;
			{ ok, S } when is_number( S ) ->	S
		end,
		statse:gauge( StatKey, Stat ),
		ok
	catch
		Type:Reason -> { error, { Type, Reason } }
	end.

-spec start_timer( #monitored{} ) -> #monitored{}.
start_timer( #monitored{ key = StatKey, refresh = RefreshPeriod } = M ) ->
	M#monitored{ timer = erlang:start_timer( RefreshPeriod, self(), StatKey ) }.

-spec remove_monitor( statse:stat_key(), [ #monitored{} ] ) -> [ #monitored{} ].
remove_monitor( StatKey, Monitored ) ->
	case lists:keytake( StatKey, #monitored.key, Monitored ) of
		{ value, #monitored{ timer = Timer }, OtherMonitored } ->
			erlang:cancel_timer( Timer ),
			OtherMonitored;
		_ ->
			Monitored
	end.
