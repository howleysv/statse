-module( statse_worker ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( gen_server ).

%% Public interface
-export( [ start_link/0, start_link/3, send_stat/2 ] ).

%% gen_server behavour
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

%% Internal State
-record( state, {
			socket 		:: gen_udp:socket(),	%% The socket from which data is sent
			ip 		:: inet:ip_address(),	%% IP of the statsd server
			port 		:: inet:port_number(),	%% Port number of the statsd server
			stat_prefix	:: statse:stat_key()	%% Prefix namespace for all stats
		} ).

-type call_type() 	:: stop.
-type cast_type()	:: { counter, statse:stat_key(), integer() }
			|  { counter, statse:stat_key(), integer(), float() }
			|  { timer, statse:stat_key(), integer() }
			|  { gauge, statse:stat_key(), integer() }
			|  { gauge_change, statse:stat_key(), integer() }.
-type info_type()	:: _.
-type call_ret() 	:: { noreply, #state{} }
			|  { reply, term(), #state{} }
			|  { stop, term(), term(), #state{} }.
-type cast_ret()	:: { noreply, #state{} }
			|  { stop, term(), #state{} }.

-spec start_link() -> { ok, pid() }.
start_link() ->
	{ ok, Host } = application:get_env( statsd_host ),
	{ ok, Port } = application:get_env( statsd_port ),
	{ ok, StatPrefix } = application:get_env( stat_prefix ),
	start_link( Host, Port, StatPrefix ).

-spec start_link( inet:hostname(), inet:port_number(), statse:stat_key() ) -> { ok, pid() }.
start_link( Host, Port, StatPrefix ) ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, { Host, Port, StatPrefix }, [] ).

-spec send_stat( atom() | pid(), tuple() ) -> ok.
send_stat( StatClient, Stat ) ->
	gen_server:cast( StatClient, Stat ).

-spec init( { inet:hostname(), inet:port_number(), statse:stat_key() } ) -> { ok, #state{} }.
init( { Host, Port, StatPrefix } ) ->
	{ ok, IP } = inet:getaddr( Host, inet ),
	{ ok, Socket } = gen_udp:open( 0, [ { active, false } ] ),
	{ ok, #state{ socket = Socket, ip = IP, port = Port, stat_prefix = build_prefix( StatPrefix ) } }.

-spec handle_call( call_type(), { pid(), term() }, #state{} ) -> call_ret().
handle_call( stop, _From, #state{} = State ) ->
	{ stop, normal, ok, State };

%% Unrecognised request
handle_call( _Request, _From, State ) ->
	error_logger:warning_msg( "Unrecognized call: ~p", [ _Request ] ),
	{ noreply, State }.

-spec handle_cast( cast_type(), #state{} ) -> cast_ret().
handle_cast( { counter, Stat, Count }, #state{} = State ) ->
	Format = io_lib:format( "~p|c", [ Count ] ),
	send_stat( Stat, Format, State ),
	{ noreply, State };

handle_cast( { counter, Stat, Count, SampleRate }, #state{} = State ) ->
	Format = io_lib:format( "~p|c|@~p", [ Count, SampleRate ] ),
	send_stat( Stat, Format, State ),
	{ noreply, State };

handle_cast( { timer, Stat, Millis }, #state{} = State ) ->
	Format = io_lib:format( "~p|ms", [ Millis ] ),
	send_stat( Stat, Format, State ),
	{ noreply, State };

handle_cast( { gauge, Stat, Value }, #state{} = State ) ->
	Format = io_lib:format( "~p|g", [ Value ] ),
	send_stat( Stat, Format, State ),
	{ noreply, State };

handle_cast( { gauge_change, Stat, Delta }, #state{} = State ) when Delta >= 0 ->
	Format = io_lib:format( "+~p|g", [ Delta ] ),
	send_stat( Stat, Format, State ),
	{ noreply, State };

handle_cast( { gauge_change, Stat, Delta }, #state{} = State ) ->
	Format = io_lib:format( "~p|g", [ Delta ] ),
	send_stat( Stat, Format, State ),
	{ noreply, State };

%% Unrecognised request
handle_cast( _Request, State ) ->
	error_logger:warning_msg( "Unrecognized cast: ~p", [ _Request ] ),
	{ noreply, State }.

-spec handle_info( info_type(), #state{} ) -> cast_ret().
%% Unrecognised message
handle_info( _Message, State ) ->
	error_logger:warning_msg( "Unrecognized message: ~p", [ _Message ] ),
	{ noreply, State }.

-spec terminate( term(), #state{} ) -> _.
terminate( _Reason, #state{ socket = Socket } ) ->
	gen_udp:close( Socket ).

-spec code_change( term(), #state{}, term() ) -> { ok, #state{} }.
code_change( _OldVersion, State, _Extra ) ->
	{ ok, State }.

-spec build_prefix( statse:stat_key() ) -> statse:stats_key().
build_prefix( [] ) -> [];

build_prefix( Prefix ) ->
	Binary = iolist_to_binary( Prefix ),
	case binary:last( Binary ) of
		$. ->	Binary;
		_ ->	<<Binary/binary, $./integer>>
	end.

-spec send_stat( statse:stat_key(), iolist(), #state{} ) -> ok.
send_stat( Stat, Data, #state{ socket = Socket, ip = IP, port = Port, stat_prefix = StatPrefix } ) ->
	Packet = iolist_to_binary( [ StatPrefix, Stat, $:, Data ] ),
	ok = gen_udp:send( Socket, IP, Port, Packet ).
