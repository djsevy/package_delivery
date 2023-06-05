%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
%% These solutions are not intended to be ideal solutions. Instead,
%% they are a solution that you can compare against yours to see
%% other options and to come up with even better solutions.
-module(request_package_server).
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% -export([get_package/1]).

-define(SERVER, ?MODULE). 

%-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?MODULE, stop).

get_friends_of(Name)-> gen_server:call(?MODULE, {friends_of,Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	%{Success, Riak_PID} = riakc_pb_socket:start_link("rdb.fordark.org", 8087).
    	case riakc_pb_socket:start_link("rdb.fordark.org", 8087) of 
	     {ok,Riak_Pid} -> {ok,Riak_Pid};
	     _ -> {stop,link_failure}
	end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({friends_of,Name}, _From, Riak_PID) ->
    	%{reply,<<bob,sue,alice>>,Riak_PID};
	case riakc_pb_socket:get(Riak_PID, <<"friends">>, Name) of 
	    {ok,Fetched}->
		%reply with the value as a binary, not the key nor the bucket.
		{reply,binary_to_term(riakc_obj:get_value(Fetched)),Riak_PID};
	     Error ->
		{reply,Error,Riak_PID}
	end;
handle_call(stop, _From, _State) ->
	{stop,normal,
                server_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================










%%%===================================================================
%%% Eunit Tests
%%%===================================================================
-ifdef(EUNIT).
 -include_lib("eunit/include/eunit.hrl").
handle_update_test_()->
    {setup,
		fun()-> 
				% meck:new(request_package_server), 
				meck:new(riak_get),
				% meck:new(request_eta), 
				% meck:expect(request_package_server, get_package, fun(Package_id) -> {vehicle, history} end),
				meck:expect(riak_get, get_package, fun(Package_id) -> {vehicle, history} end),
				meck:expect(riak_get, get_vehicle, fun(Vehicle_id) -> {lat, lon} end)
				% meck:expect(request_eta, new, fun(Package_id) -> eta end)


		end,
		fun(_)-> 
			% meck:unload(request_package_server),
			meck:unload(riak_get)
			% meck:unload(request_eta)
		end,
	[
        ?_assertEqual({reply,
            {lat, lon, eta, history}},
        update_location_server:handle_call(request_location, somewhere, {"123"})),

        ?_assertThrow({badcommand,
			mojave_desert},
        update_location_server:handle_call(mojave_desert, somewhere, {"123"})),

		?_assertError({badarg,
			{"123", "456"}},
        update_location_server:handle_call(request_location, somewhere, {"123", "456"}))
	]}.
-endif.