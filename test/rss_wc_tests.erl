%% @author dkelsey
%% @doc @todo Add description to rss_wc_tests.


-module(rss_wc_tests).
-include("global.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
%-export([]).


decode_uri_test() ->
	% rss_wc:decode_uri(<<"http%3A%2F%2Fdigg.com%2Frss%2Ftop.rss">>).
	% rss_wc:decode_uri(<<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>).
	Uri = <<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>,
	{ok, DecodedUri} = rss_wc:do_decode_uri(Uri),
	?assert( erlang:length(DecodedUri) > 0).

% This test need inets to be started.
get_uri_test() ->
	URI = "http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml",
	inets:start(), % a hack
	{ok, Body} = rss_wc:do_get_uri(URI),
	?assert(erlang:length(Body) > 0).

parse_xml_test() ->
	{ok, XMLBody} = file:read_file("test/data/NYT_HomePage.xml"),
	{ok, Text} = rss_wc:do_parse_xml(binary_to_list(XMLBody), "//item/description/text()"),
%	?debugFmt("XMLBody length: ~b~n", [erlang:length(binary_to_list(XMLBody))]),
%	?debugFmt("Text length: ~b~n", [erlang:length(Text)]),
	?assert(erlang:length(Text) > 5).

parse_text_test() -> 
	{ok, Text} = file:read_file("test/data/TextData.txt"),
	{ok, FilteredText} = rss_wc:do_parse_text(binary_to_list(Text)),
%	?debugFmt("~nText length: ~p", [erlang:length(binary_to_list(Text))]),
%	?debugFmt("~nFilteredText length: ~b", [erlang:length(FilteredText)]),
%	?debugFmt("Text: ~s~n", [Text]),
%	?debugFmt("NewTokens ~s~n", [NewTokens]),
	?assert(erlang:length(FilteredText) > 0),
	?assertEqual(erlang:length(binary_to_list(Text)), erlang:length(FilteredText)).

tokenize_text_test() ->
	{ok, Text} = file:read_file("test/data/ParsedText.txt"),
	{ok, Tokens} = rss_wc:do_tokenize_text(binary_to_list(Text), " ="),
%	?debugFmt("Text length: ~b~n", [erlang:length(binary_to_list(Text))]),
%	?debugFmt("Tokens length: ~b~n", [erlang:length(Tokens)]),
	?assert(erlang:length(Tokens) > 0).

filter_stopwords_test() -> ok.
count_tokens_test() -> ok.
sort_tokens_test() -> ok.
limit_tokens_test() -> ok.
format_to_json_test() -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


