-module( statse_worker ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-behaviour( gen_server ).

%% Public interface
-export( [ start_link/0, start_link/2 ] ).

%% gen_server behavour
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

%% Internal State
-record( state, {
			socket 	:: gen_udp:socket(),	%% The socket from which data is sent
			ip 	:: inet:ip_address(),	%% IP of the statsd server
			port 	:: inet:port_number()	%% Port number of the statsd server
		} ).

-type call_type() 	:: stop.
-type cast_type()	:: stop.
-type info_type()	:: stop.
-type call_ret() 	:: { noreply, #state{} }
			|  { reply, term(), #state{} }
			|  { stop, term(), term(), #state{} }.
-type cast_ret()	:: { noreply, #state{} }
			|  { stop, term(), #state{} }.

-spec start_link() -> { ok, pid() }.
start_link() ->
	{ ok, Host } = application:get_env( statsd_host ),
	{ ok, Port } = application:get_env( statsd_port ),
	start_link( Host, Port ).

-spec start_link( inet:hostname(), inet:port_number() ) -> { ok, pid() }.
start_link( Host, Port ) ->
	gen_server:start_link( ?MODULE, { Host, Port }, [] ).

-spec init( { inet:hostname(), inet:port_number() } ) -> { ok, #state{} }.
init( { Host, Port } ) ->
	{ ok, IP } = inet:getaddr( Host, inet ),
	{ ok, Socket } = gen_udp:open( 0, [ { active, false } ] ),
	{ ok, #state{ socket = Socket, ip = IP, port = Port } }.

-spec handle_call( call_type(), { pid(), term() }, #state{} ) -> call_ret().
handle_call( stop, _From, #state{} = State ) ->
	{ stop, normal, ok, State };

%% Unrecognised request
handle_call( _Request, _From, State ) ->
	error_logger:warning_msg( "Unrecognized call: ~p", [ _Request ] ),
	{ noreply, State }.

-spec handle_cast( cast_type(), #state{} ) -> cast_ret().
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
