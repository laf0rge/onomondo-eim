-module(esipa_initiateAuthentication_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
	{ok, Req, State}.
