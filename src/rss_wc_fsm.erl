-module(rss_wc_fsm).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).

%% gen_fsm.
-export([init/1]).
-export([idle/2]).
-export([handle_event/3]).
-export([idle/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-export([decode_uri/1]).
-export([decoded_uri/1]).
-export([decoding/2]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

decode_uri(URI) ->
	gen_fsm:send_event(?MODULE, {decode_uri, URI}).
decoded_uri(DecodedURI) ->
	gen_fsm:send_event(?MODULE, {decoded, DecodedURI}).

%% gen_fsm.

init([]) ->
	{ok, idle, #state{}}.

idle({decode_uri, URI}, StateData) ->
	{decoded, _DecodedUri } = rss_wc:decode_uri(URI), % rss_wc should respond with a message to rss_wc_fsm ! {decoded DecodedURI}
%    {reply, URI, idle, StateData};
%	io:format("URI: ~p", [URI]),
	{next_state, decoding, StateData};
idle(_Event, StateData) ->
	{next_state, idle, StateData}.

decoding({decoded, DecodedURI}, StateData) ->
	io:format("DecodedURI: ~p", [DecodedURI]),
	{reply, DecodedURI, idle, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

idle(_Event, _From, StateData) ->
	{reply, ignored, idle, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
