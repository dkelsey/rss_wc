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
* parses (normalizes) the resulting text into lower case
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
| xml_search_path | "//items/description/text()" | the filed to use for the word count |
| limit | 10 | the amount to limit by in the result e.g. top 10 |

 
#Installation

```
mkdir project
cd project
git clone https://github.com/dkelsey/rss_wc.git
cd rss_wc
make clean app
...
...

erl -pa ./ebin -pa ./deps/*/ebin
Erlang/OTP 17 \[erts-6.3] \[source-f9282c6] \[64-bit] \[smp:4:4] \[async-threads:10] \[hipe] \[kernel-poll:false]

Eshell V6.3  (abort with ^G)
1> application:start(jiffy),applicaion:start(rss_wc).
ok
2> rss_wc:decode_uri(<<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>).
ok
3> flush().
Shell got {#Ref<0.0.0.58>,
           {ok,<<"{\"words\":[{\"word\":\"border\",\"count\":65},{\"word\":\"src\",\"count\":65},{\"word\":\"'1'\",\"count\":40},{\"word\":\"href\",\"count\":36},{\"word\":\"height\",\"count\":29},{\"word\":\"width\",\"count\":29},{\"word\":\"\\\"nofollow\\\"><img\",\"count\":27},{\"word\":\"rel\",\"count\":27},{\"word\":\"'0'/><br\",\"count\":20},{\"word\":\"clear\",\"count\":20}],\"stopWordsIgnored\":175}">>}}
ok
4>
```

#Usage
 
```
erl -pa ./ebin -pa ./deps/*/ebin
Erlang/OTP 17 \[erts-6.3] \[source-f9282c6] \[64-bit] \[smp:4:4] \[async-threads:10] \[hipe] \[kernel-poll:false]

Eshell V6.3  (abort with ^G)
1> application:start(jiffy), application:start(rss_wc).
ok
2> rss_wc:decode_uri(<<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>).
ok
3> flush().
Shell got {#Ref<0.0.0.58>,
           {ok,<<"{\"words\":[{\"word\":\"border\",\"count\":65},{\"word\":\"src\",\"count\":65},{\"word\":\"'1'\",\"count\":40},{\"word\":\"href\",\"count\":36},{\"word\":\"height\",\"count\":29},{\"word\":\"width\",\"count\":29},{\"word\":\"\\\"nofollow\\\"><img\",\"count\":27},{\"word\":\"rel\",\"count\":27},{\"word\":\"'0'/><br\",\"count\":20},{\"word\":\"clear\",\"count\":20}],\"stopWordsIgnored\":175}">>}}
ok
```

#Dependencies
 
* jiffy

#Todo