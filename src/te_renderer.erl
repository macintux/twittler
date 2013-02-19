%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2013 by John Daily <jd@epep.us>

-module(te_renderer).
-compile(export_all).

%% te_helper:timeline(user, {count, 3}, fun(X) -> lists:map(fun chaseurls:chase/1, te_helper:extract_urls(X)) end).

render_timeline(What, Count, Renderer) ->
    te_helper:timeline(What, {count, Count}, fun(X) -> render_tweet(te_helper:extract_retweet(X), Renderer) end).

render_tweet(Tweet, Renderer) ->
    %% We want: URLs, page title for each URL, tweet body, author, tweet url
    Author = te_helper:author_details(Tweet),
    URLs = lists:filter(fun(X) -> X =/= undefined end,
                        lists:map(fun(X) -> check_chase(chaseurls:chase(X)) end,
                                  te_helper:extract_urls(Tweet))),
    io_lib:format("~ts~n", [Renderer([
              {tweet, Tweet},
              {body, proplists:get_value(<<"text">>, Tweet, <<"">>)},
              {urls, URLs},
              {author, Author}])]).

simple_html_renderer(Details) ->
    [<<"<div><p>">>, proplists:get_value(body, Details), <<"<br>- <a href='">>,
     te_helper:author_twitter_url(proplists:get_value(tweet, Details)),
     <<"'>">>,
     proplists:get_value(name, proplists:get_value(author, Details)),
     <<" (@">>,
     proplists:get_value(screen_name, proplists:get_value(author, Details)),
     <<")</a>">>,
     <<"</p>">>, list_urls(proplists:get_value(urls, Details)), <<"</div><hr>">>
    ].

list_urls([]) ->
    <<"">>;
list_urls(List) ->
    [<<"<ul>">>, url_list_items(List, []), <<"</ul>">>].

url_list_items([], Accum) ->
    Accum;
url_list_items([H|T], Accum) ->
    url_list_items(T, [<<"<li><a href='">>, H, <<"'>">>, H, <<"</a></li>">>, Accum]).

%%
%% If chaseurls:chase() returns ok with a list, the head of the list
%% is the final URL. Otherwise, it returns brokenchain or toomany with
%% the list of URLs it found; grab the tail of the list, the URL from
%% the tweet.
check_chase({ok, [H|_T]}) ->
    H;
check_chase({_Error, [H|_T] = List}) when length(List) > 0 ->
    H;
check_chase({_Error, _}) ->
    undefined.
