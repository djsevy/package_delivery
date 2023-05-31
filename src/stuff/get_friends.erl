%% @doc A handler to deal with retrieval of the list of friends
%% for a specific individual.
-module(get_friends).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	[Name|_] = jsx:decode(Data),
	Friends = jsx:encode(get_friends_server:get_friends_of(Name)),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Friends, Req0),
	{ok, Req, Opts}.
