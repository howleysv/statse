-module( statse_worker ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( gen_server ).

%% Public interface
-export( [ start_link/0, start_link/3, stop/1, send_stat/2 ] ).

%% gen_server behavour
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

%% Internal State
-record( state, {
			socket 		:: inet:socket(),	%% The socket from which data is sent
			ip 		:: inet:ip_address(),	%% IP of the statsd server
			port 		:: inet:port_number(),	%% Port number of the statsd server
			stat_prefix	:: statse:stat_key()	%% Prefix namespace for all stats
		} ).

-type stat_type()	:: counter
			|  timer
			|  gauge
			|  gauge_change
			|  set.

-type call_type() 	:: stop.
-type cast_type()	:: { stat_type(), statse:stat_key(), number() | iodata() }
			|  { stat_type(), statse:stat_key(), number() | iodata(), float() }.
-type info_type()	:: _.
-type call_ret() 	:: { noreply, #state{} }
			|  { stop, term(), term(), #state{} }.
-type cast_ret()	:: { noreply, #state{} }.

-spec start_link() -> { ok, pid() }.
start_link() ->
	{ ok, Host } = application:get_env( statsd_host ),
	{ ok, Port } = application:get_env( statsd_port ),
	{ ok, StatPrefix } = application:get_env( stat_prefix ),
	start_link( Host, Port, StatPrefix ).

-spec start_link( inet:hostname(), inet:port_number(), statse:stat_key() ) -> { ok, pid() }.
start_link( Host, Port, StatPrefix ) ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, { Host, Port, StatPrefix }, [] ).

-spec stop( atom() | pid() ) -> ok | { error, term() }.
stop( StatClient ) ->
	gen_server:call( StatClient, stop ).

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
	{ stop, normal, ok, State }.

-spec handle_cast( cast_type(), #state{} ) -> cast_ret().
handle_cast( { gauge, StatKey, Value }, #state{} = State ) when Value < 0 ->
	{ noreply, NewState } = handle_cast( { gauge, StatKey, 0 }, State ),
	handle_cast( { gauge_change, StatKey, Value }, NewState );

handle_cast( { StatType, StatKey, Value }, #state{} = State ) ->
	StatData = build_stat( StatType, Value ),
	send_stat( StatKey, StatData, State ),
	{ noreply, State };

handle_cast( { StatType, StatKey, Value, SampleRate }, #state{} = State ) ->
	StatData = build_stat( StatType, Value, SampleRate ),
	send_stat( StatKey, StatData, State ),
	{ noreply, State };

%% Unrecognised request
handle_cast( _Request, State ) ->
	error_logger:warning_msg( "Malformed stat: ~p", [ _Request ] ),
	{ noreply, State }.

-spec handle_info( info_type(), #state{} ) -> cast_ret().
%% Unrecognised message
handle_info( _Message, State ) ->
	{ noreply, State }.

-spec terminate( term(), #state{} ) -> _.
terminate( _Reason, #state{ socket = Socket } ) ->
	gen_udp:close( Socket ).

-spec code_change( term(), #state{}, term() ) -> { ok, #state{} }.
code_change( _OldVersion, State, _Extra ) ->
	{ ok, State }.

-spec build_prefix( statse:stat_key() ) -> statse:stat_key().
build_prefix( [] ) -> [];

build_prefix( Prefix ) ->
	Binary = iolist_to_binary( Prefix ),
	case binary:last( Binary ) of
		$. ->	Binary;
		_ ->	<<Binary/binary, $./integer>>
	end.

-spec build_stat( stat_type(), number() | iodata() ) -> iolist().
build_stat( gauge_change, Value ) when Value >= 0 ->	[ $+, build_stat( gauge, Value ) ];
build_stat( set, Value ) when not is_number( Value ) ->	[ io_lib:format( "~s", [ Value ] ), $|, encode_stat_type( set ) ];
build_stat( StatType, Value ) ->			[ io_lib:format( "~w", [ Value ] ), $|, encode_stat_type( StatType ) ].

-spec build_stat( stat_type(), number(), float() ) -> iolist().
build_stat( StatType, Value, SampleRate ) ->		[ build_stat( StatType, Value ), "|@", io_lib:format( "~w", [ SampleRate ] ) ].

-spec encode_stat_type( stat_type() ) -> iolist().
encode_stat_type( counter ) ->		"c";
encode_stat_type( timer ) ->		"ms";
encode_stat_type( gauge ) ->		"g";
encode_stat_type( gauge_change ) ->	"g";
encode_stat_type( set ) ->		"s".

-spec send_stat( statse:stat_key(), iolist(), #state{} ) -> ok.
send_stat( Stat, Data, #state{ socket = Socket, ip = IP, port = Port, stat_prefix = StatPrefix } ) ->
	Packet = iolist_to_binary( [ StatPrefix, Stat, $:, Data ] ),
	ok = gen_udp:send( Socket, IP, Port, Packet ).
