-module(rss_wc_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	rss_wc_sup:start_link().

stop(_State) ->
	ok.
