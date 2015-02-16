-module(rss_wc).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([shutdown/0]).
-export([decode_uri/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([debug_info,export_all]).
-endif.

-include_lib("xmerl/include/xmerl.hrl").

-record(state, { token_string=" ",
			     limit=10,
				 search_path="//item/description/text()"
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE }, ?MODULE, [], []).

shutdown() ->
	gen_server:call(?MODULE, stop).
decode_uri(Uri) ->
	gen_server:call(?MODULE, {decode_uri, Uri}).

%% gen_server.

init([]) ->
	ets:new(cache, [set, named_table]),
%	inets:start(), % This was causing the release to fail to start.
	{ok, TokenString} = application:get_env(?MODULE, token_string),
	{ok, Limit} = application:get_env(?MODULE, count_limit),
	{ok, SearchPath} = application:get_env(?MODULE, search_path),
	{ok, StopwordsFile} = application:get_env(?MODULE, stopwords_file),
	{ok, StopWords} = file:read_file(StopwordsFile),
	ets:new(stopwords, [set, named_table]),
	StopTokens = string:tokens( binary_to_list(StopWords), "\n"),
	StopWordsList = [{T,0} || T <- StopTokens],
	ets:insert(stopwords, StopWordsList),
	{ok, #state{token_string=TokenString, limit=Limit, search_path=SearchPath}}.

handle_call({decode_uri, Uri}, From, State) ->
	case do_check_cache(Uri) of
		{absent} ->
			{ok, DecodedUri} = do_decode_uri(Uri),
			gen_server:cast(?MODULE, {get_uri, Uri, DecodedUri, From});
		{ok, {_CachedURI, JSONString}} ->
			gen_server:cast(?MODULE, {reply,  JSONString, From})
	end,
	{reply, ok, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({get_uri, Uri, DecodedUri, From}, State) ->
	{ok, XMLBody} = do_get_uri(DecodedUri),
	gen_server:cast(?MODULE, {parse_xml, Uri, XMLBody, From}),
	{noreply, State};
handle_cast({parse_xml, Uri, XMLBody, From}, State) ->
	{ok, Text} = do_parse_xml(XMLBody, State#state.search_path),
	gen_server:cast(?MODULE, {parse_text, Uri, Text, From}),
	{noreply, State};
handle_cast({parse_text, Uri, Text, From}, State) ->
	{ok, FilteredText} = do_parse_text(Text),
	gen_server:cast(?MODULE, {tokenize_text, Uri, FilteredText, From}),
	{noreply, State};
handle_cast({tokenize_text, Uri, Text, From}, State) ->
	{ok, FilteredText} = do_tokenize_text(Text, State#state.token_string),
	gen_server:cast(?MODULE, {filter_stopwords, Uri, FilteredText, From}),
	{noreply, State};
handle_cast({filter_stopwords, Uri, Text, From}, State) ->
	{ok, FilteredTokens, StopwordCounts} = do_filter_stopwords(Text),
	gen_server:cast(?MODULE, {count_tokens, Uri, FilteredTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({count_tokens, Uri, FilteredTokens, StopwordCounts, From}, State) ->
	{ok, CountedTokens} = do_count_tokens(FilteredTokens),
	gen_server:cast(?MODULE, {sort_tokens, Uri, CountedTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({sort_tokens, Uri, CountedTokens, StopwordCounts, From}, State) ->
	{ok, SortedTokens} = do_sort_tokens(CountedTokens),
	gen_server:cast(?MODULE, {limit_tokens, Uri, SortedTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({limit_tokens, Uri, Tokens, StopwordCounts, From}, State) ->
	{ok, LimitedTokens} = do_limit_tokens(Tokens, State#state.limit),
	gen_server:cast(?MODULE, {format_to_json, Uri, LimitedTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({format_to_json, Uri, Tokens, StopwordCounts, From}, State) ->
	{ok, JSON_String} = do_format_to_json(Tokens, StopwordCounts),
	gen_server:cast(?MODULE, {cache, Uri, JSON_String, From}),
	{noreply, State};
handle_cast({cache, Uri, JSON_String, From}, State) ->
	{ok, cached} = do_cache(Uri, JSON_String),
	gen_server:cast(?MODULE, {reply, JSON_String, From}),
	{noreply, State};
handle_cast({reply, JSON_String, From}, State) ->
	gen_server:reply(From, {ok, JSON_String}),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("performing some cleanup~n"),
	ets:delete(cache),
	ets:delete(stopwords),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% Private functions
do_check_cache(Uri) ->
	case ets:lookup(cache, Uri) of
		[] -> {absent};
		[JSONString] -> {ok, JSONString}
	end.
do_cache(Uri, JSONString) ->
	ets:insert(cache, {Uri, JSONString}),
	{ok, cached}.
do_decode_uri(Uri) ->
	DecodedUri = http_uri:decode(binary_to_list(Uri)),
	{ok, DecodedUri}.
do_get_uri(DecodedUri) ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, XMLBody}} = httpc:request(DecodedUri),
	{ok, XMLBody}.
do_parse_xml(XMLBody, SearchPath) ->
	{XML_Body, _RemainingText = "" } = xmerl_scan:string(XMLBody),	
	XML_Items = xmerl_xpath:string(SearchPath, XML_Body),
	Text = lists:concat(lists:map(fun(XmlText) -> #xmlText{value=TextValue} = XmlText, string:to_lower(unicode:characters_to_list(TextValue, utf8)) end, XML_Items)),
	{ok, Text}.
do_parse_text(Text) ->
	LowerCase = string:to_lower(Text),
	FilteredText = lists:map(fun(C) -> case (C > 255) of true -> 65; _ -> C end end, LowerCase),
	{ok, string:to_lower(FilteredText)}.
do_tokenize_text(Text, TokenString) ->
%	{ok, TokenString} = application:get_env(?MODULE, token_string),
	Tokens = string:tokens(Text, TokenString),
	{ok, Tokens}.
do_filter_stopwords(Tokens) ->
%  Copy the stopwords ets set [{"a",0},{"b",0},{"c",0},...]
	StopWords = ets:tab2list(stopwords),
%	NewStopWords = [{K,0} || {K,_} <- StopWords],
	TempTable = ets:new(mystopwords, [set]),
	ets:insert(TempTable, StopWords),
	F = fun(X) -> case ets:lookup(TempTable, X) of [] -> [X]; [{X,V}] -> ets:insert(TempTable, {X,V+1}), [] end end,
	StrippedTokens = lists:flatmap(F, Tokens),
	StopwordCounts = ets:tab2list(TempTable),
	ets:delete(TempTable), 
	{ok, StrippedTokens, StopwordCounts}.
do_count_tokens(Tokens) ->
	ets:new(group, [set, named_table]),
	Munge = fun(X) -> case ets:lookup(group, X) of [] -> ets:insert(group, [{X, 1}]), []; [{X, V}] -> ets:insert(group, [{X, V+1}]), [] end end,
	_ = lists:flatmap(Munge, Tokens),
	CountedTokens  = ets:tab2list(group),
	ets:delete(group),
	{ok, CountedTokens}.
do_sort_tokens(CountedTokens) ->
% example CountedTokens = [{"src", 75}, {"border",75}, {"href",44}...]
	TempTable = ets:new(groupedsorted, [ordered_set]),
	SortByValFun = fun({Key, Val}) -> case ets:lookup(TempTable, Val) of [] -> ets:insert(TempTable, [{Val, [Key]}]), []; [{Val, Vec}] -> ets:insert(TempTable, [{Val, [Key|Vec]}]), [] end end,
	_ = lists:flatmap(SortByValFun, CountedTokens),
	InverseList = ets:tab2list(TempTable),
	ets:delete(TempTable),
% InverseList should now be [{75, ["src","border"]}, {44, ["href","meow",...]},...]
% InverseList will be sorted by Key.
	SortByKeyFun = fun({A, _Al} ,{B, _Bl}) -> A > B end,
	SortedInverseList = lists:sort(SortByKeyFun,InverseList),
% SortedInverseList is now sorted by Key
% now sort the val's in the list
	SortValuesFun = fun({Key, Val}) -> {Key, lists:sort(Val)} end,
	SortedInverseAndValueList = lists:map(SortValuesFun, SortedInverseList),
	PivotKeys = fun({Key,Vals}) -> lists:map(fun(X) -> {X, Key} end, Vals) end,
	TokenCountSortedLists = lists:map(PivotKeys, SortedInverseAndValueList),
	SortedCounts = lists:append(TokenCountSortedLists),
	{ok, SortedCounts}.
do_limit_tokens(Counts, CountLimit) ->
	LimitedCounts = lists:sublist(Counts, CountLimit),
	{ok, LimitedCounts}.
do_format_to_json(SortedTokens, StopwordsCount) ->
%	jiffy:decode(<<"{\"words\":[{\"word\":\"foo\",\"count\":1000},{\"word\":\"bar\",\"count\":5000}],\"stopWordsIgnored\":10000}">>).
%{[{<<"words">>,
%   [{[{<<"word">>,<<"foo">>},{<<"count">>,1000}]},
%    {[{<<"word">>,<<"bar">>},{<<"count">>,5000}]}]},
%  {<<"stopWordsIgnored">>,10000}]}
%
%	SortList = [{"first",5},{"third",4},{"second",4},{"sixth",1},{"fifth",1},{"fourth",1}].
	BlowUp = fun({Wd,Val}) -> [{[{<<"word">>,list_to_binary(Wd)},{<<"count">>,Val}]}] end,
	SortedList_in_Json = lists:flatmap(BlowUp, SortedTokens),
% total stopwords - maybe do this long before?
	StopwordsIgnoredTotal = lists:foldl(fun({_, X}, Sum) -> X + Sum end, 0, StopwordsCount),
	Final_Json={[{<<"words">>,SortedList_in_Json},{<<"stopWordsIgnored">>,StopwordsIgnoredTotal}]},
	JSON_string = jiffy:encode(Final_Json),
	{ok, JSON_string}.

