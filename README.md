#rss_wc 
========================================

**rss_wc** is an erlang application which accepts a uuencoded URI to an XML doc and returns a top 10 word count in JSON.
 

#Details

**rss_wc** is composed of a single worker (gen_server behavior), a supervisor and application module.
The worker is called with the URI.  There are several stages of processing performed until finally the JSON string is returned.
The worker performs each stage in sequence by submitting asynchronous calls to itself passing relevant parameters: each stage, after performing it's function, passes relevant data to the next stage.

**rss_wc** performs the following stages:

* determine if the URI has been cached and return the cached result, else
* decodes the URI
* gets the XML using `inets/httpc`
* parses the XML using `xmerl` and extracts the relevant data as text
* parses (normalizes) the resulting text into lower case and filter chars above 255 to '?' (65) 
* tokenize the text 
* filters the resulting tokens from a stopwords list : the count of filtered words is used later
* counts the occurrence of each word
* sorts the list from highest count to lowers and alphabetically within similar counts
* limits the list.
* format the list and count of filtered words in JSON
* cache the results using the URI as the key
* respond to the client with the JSON String

Some settings can be modified in the application configuration.  
These are:

| setting | value | meaning |
| ------- | ----- | ------- |
| stopwords_file | "stopwords.txt" | file containing stopwords |
| token_string | "= " | the characters to tokenize text data on |
| search_path | "//item/description/text()" | the filed to use for the word count |
| count_limit | 10 | the amount to limit by in the result e.g. top 10 |

 
#Installation

```
mkdir project
cd project
git clone https://github.com/dkelsey/rss_wc.git
cd rss_wc
make clean app
make deps
...
...

erl -pa ./ebin -pa ./deps/*/ebin
Erlang/OTP 17 [erts-6.3] [source-f9282c6] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.3  (abort with ^G)
1> application:start(jiffy).
ok
2> applicaion:start(inets).
ok
3> applicaion:start(xmerl).
ok
4> applicaion:start(rss_wc).
ok
5> rss_wc:decode_uri(<<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>).
ok
6> flush().
Shell got {#Ref<0.0.0.58>,
           {ok,<<"{\"words\":[{\"word\":\"border\",\"count\":65},{\"word\":\"src\",\"count\":65},{\"word\":\"'1'\",\"count\":40},{\"word\":\"href\",\"count\":36},{\"word\":\"height\",\"count\":29},{\"word\":\"width\",\"count\":29},{\"word\":\"\\\"nofollow\\\"><img\",\"count\":27},{\"word\":\"rel\",\"count\":27},{\"word\":\"'0'/><br\",\"count\":20},{\"word\":\"clear\",\"count\":20}],\"stopWordsIgnored\":175}">>}}
ok
7>
```

After the installation you can run unit tests as follows:
```
make tests
 GEN    clean-app
./rebar compile
==> proper (compile)
make[2]: `include/compile_flags.hrl' is up to date.
==> jiffy (compile)
 ERLC   rss_wc.erl rss_wc_app.erl rss_wc_sup.erl
 APP    rss_wc.app.src
 GEN    test-dir
 GEN    test-build
 GEN    eunit
======================== EUnit ========================
directory "ebin"
  module 'rss_wc_sup'
  module 'rss_wc_app'
  module 'rss_wc'
    module 'rss_wc_tests'
      rss_wc_tests: decode_uri_test...[0.002 s] ok
      rss_wc_tests: get_uri_test...[0.503 s] ok
      rss_wc_tests: parse_xml_test...[0.062 s] ok
      rss_wc_tests: parse_text_test...[0.006 s] ok
      rss_wc_tests: tokenize_text_test...[0.003 s] ok
      rss_wc_tests: filter_stopwords_test...ok
      rss_wc_tests: count_tokens_test...ok
      rss_wc_tests: sort_tokens_test...ok
      rss_wc_tests: limit_tokens_test...ok
      rss_wc_tests: format_to_json_test...ok
      [done in 0.605 s]
    [done in 0.605 s]
  [done in 0.621 s]
=======================================================
  All 10 tests passed.
 GEN    triq
Undefined property or module
```

#Usage
 
```
erl -pa ./ebin -pa ./deps/*/ebin
Erlang/OTP 17 [erts-6.3] [source-f9282c6] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.3  (abort with ^G)
1> application:start(jiffy).
ok
2> applicaion:start(inets).
ok
3> applicaion:start(xmerl).
ok
4> applicaion:start(rss_wc).
ok
5> rss_wc:decode_uri(<<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>).
ok
6> flush().
Shell got {#Ref<0.0.0.58>,
           {ok,<<"{\"words\":[{\"word\":\"border\",\"count\":65},{\"word\":\"src\",\"count\":65},{\"word\":\"'1'\",\"count\":40},{\"word\":\"href\",\"count\":36},{\"word\":\"height\",\"count\":29},{\"word\":\"width\",\"count\":29},{\"word\":\"\\\"nofollow\\\"><img\",\"count\":27},{\"word\":\"rel\",\"count\":27},{\"word\":\"'0'/><br\",\"count\":20},{\"word\":\"clear\",\"count\":20}],\"stopWordsIgnored\":175}">>}}
ok
7>
```

#Dependencies
 
The installation process above covers the dependencies.  I'm listing them here for completeness.

* jiffy
* inets
* xmerl

#Notes

The eunit tests are pedantic at this point.  More valid test should be added.
I have ideas on how to scale the app.  At this point there is a single gen_server worker running on the same host.
I envision the next step would be to move the worker to a different host, taking advantage of another core.
The next step after that, i'd envision, further devision and distribution: create pools of workers and a work queue.   Before doing any of this I would want to setup an automated deployment environment (the environment is deployable) containing all necessary tools for producing load, recording metrics. 

#Questions

* How does the app handle failures?

Not implemented.  Each step in the processing is performed in it's own function.  I intended to add checking for bad data and reasonable responses.

* How does the app perform under load?

Not yet implemented.  Idealy it would be tested under tsung.  This would require the setup of metering to gather performance data points.

* How does the app perform logging and how does that affect performance?

no logging yet.  Writing log files on a system that has to perform is not a good strategy.  I envisioned using a client that sent logs over UDP to a centralized logging server.  I've used Logstash/Redis/Elasticsearc/Kiban for this.  This would take time to do.  Understanding the performance impact would again require metering.

* How does the app collect 	and expose stats/metrics?

not implemented.  I intended to add [StatsD](https://github.com/etsy/statsd/) -- A client pushing out UDP to a centralized collection server.  Further I would use Elasticsearch et al to gather and present logging data.

#Todo

* integrate StatsD
* add logging as described above
* perform more thorough testing and fill out unit tests.
* add a basic automated acceptance test : this was started but not completed.
* perform load testing 