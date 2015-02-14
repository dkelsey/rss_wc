-module(rss_wc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{rss_wc, 
			{rss_wc, start_link, []},
			permanent, 
			1000, 
			worker, 
			[rss_wc]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
