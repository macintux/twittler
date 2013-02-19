%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%    Utilities for smarter Twitter API handling. The core library is
%%%    closer to the raw Twitter API.
%%%
%%%    The core Twitter library must already be initialized.
%%% @end
%%% Created : 11 Dec 2012 by John Daily <jd@epep.us>

-module(te_helper).
-export([timeline/3, extract_urls/1, extract_mentions/1, extract_retweet/1, author_details/1, search/3, tweet_url/1, author_twitter_url/1]).

-export([test_timeline/0, test_search/0]).

-define(MAX_TL_REQ, 200).
-define(MAX_SEARCH_REQ, 100).

%% A search is effectively same as a timeline, but the results aren't structured the same.
%% Search results: [{"statuses", [<list of tweets>]}, {search_metadata, [<list_of_metadata>]}]
%% Timeline results: [<list of tweets>]

-record(state, {
          'query', %% Which timeline we want, or search query. Need better word
          api_call, %% twittler:timeline or twittler:search
          results_parser, %% search and timeline return different types of lists
          per_msg_fun, %% Client-provided function to run against each message
          max_id=0, %% First message we encounter
          min_id=0, %% Last message we encounter
          args=[] %% Arguments to pass, not including count, max_id
         }).

%% @doc Take the specified search, return the specified number of
%% messages as processed by Fun
search(Query, {count, X}, Fun) when X > ?MAX_SEARCH_REQ ->
    {State, List} =
        unified(X, ?MAX_SEARCH_REQ, [], [],
            #state{'query' = Query,
                   api_call = fun twittler:search/2,
                   results_parser = fun(Y) -> proplists:get_value(<<"statuses">>, Y) end,
                   per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ];
search(Query, {count, X}, Fun) ->
    {State, List} =
        unified(X, X, [], [],
            #state{'query' = Query,
                   api_call = fun twittler:search/2,
                   results_parser = fun(Y) -> proplists:get_value(<<"statuses">>, Y) end,
                   per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ].


timeline(Which, {count, X}, Fun) when X > ?MAX_TL_REQ ->
    {State, List} =
        unified(X, ?MAX_TL_REQ, [], [],
            #state{'query' = Which,
                   api_call = fun twittler:timeline/2,
                   results_parser = fun(Y) -> Y end,
                   per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ];
timeline(Which, {count, X}, Fun) ->
    {State, List} =
        unified(X, X, [], [],
            #state{'query' = Which,
                   api_call = fun twittler:timeline/2,
                   results_parser = fun(Y) -> Y end,
                   per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ].



find_id(Tweet) ->
    proplists:get_value(<<"id">>, Tweet).

%% @doc Pull embedded URLs from a tweet. Always check for retweets
extract_urls(Tweet) ->
    Entities = proplists:get_value(<<"entities">>, extract_retweet(Tweet)),
    URLs = proplists:get_value(<<"urls">>, Entities),
    [ proplists:get_value(<<"url">>, X) || X <- URLs ].

%% @doc Pull mentions from a tweet. Always check for retweets
%% Returns a list of {Name, ScreenName} pairs
extract_mentions(Tweet) ->
    Entities = proplists:get_value(<<"entities">>, extract_retweet(Tweet)),
    Mentions = proplists:get_value(<<"user_mentions">>, Entities),
    [ {proplists:get_value(<<"name">>, X), proplists:get_value(<<"screen_name">>, X)} || X <- Mentions ].

%% @doc Pull author details from a tweet. Always check for retweets
%% Returns { Name, ScreenName, Description, URL }
author_details(Tweet) ->
    User = proplists:get_value(<<"user">>, extract_retweet(Tweet)),
    [ {name, proplists:get_value(<<"name">>, User)},
      {screen_name, proplists:get_value(<<"screen_name">>, User)},
      {description, proplists:get_value(<<"description">>, User)},
      {url, proplists:get_value(<<"url">>, User)} ].

%% @doc Pull embedded native retweet if present
extract_retweet(Tweet) ->
    extract_retweet(Tweet, proplists:get_value(<<"retweeted_status">>, Tweet)).

extract_retweet(Tweet, undefined) ->
    Tweet;
extract_retweet(_Tweet, Retweet) ->
    Retweet.


%% Consolidate timeline, search
%% unified() returns { State, List }

%% Entry point
unified(Remaining, Requested, [], [], State) ->
    Tweets = do_call(Requested, State),
    FirstId = find_id(lists:nth(1, Tweets)),
    unified(Remaining - Requested, Requested, Tweets, [],
            State#state{max_id=FirstId});

%% Asked for more than we received, stop.
unified(_Remaining, Requested, Latest, Accum, State) when Requested > length(Latest) ->
    process_latest(Latest, Accum, State);

%% Typical ending point, no more to ask for.
unified(Remaining, _Requested, Latest, Accum, State) when Remaining =< 0 ->
    process_latest(Latest, Accum, State);

%% Last batch. Ask only for the remainder, not the max count
unified(Remaining, Requested, Latest, Accum, State) when Remaining =< Requested ->
    {NewState, NewAccum} = process_latest(Latest, Accum, State),
    unified(0, Remaining,
            do_call(Remaining, NewState), NewAccum, NewState);

%% Intermediate invocation when count > Twitter max allowed
unified(Remaining, Requested, Latest, Accum, State) ->
    {NewState, NewAccum} = process_latest(Latest, Accum, State),
    unified(Remaining - Requested, Requested,
            do_call(Requested, NewState),
            NewAccum, NewState).


do_call(NextQty, State) ->
    do_call(NextQty, State, State#state.min_id).

%% If we don't have a minimum ID yet, that means we start at the first
%% tweet Twitter will give us.
do_call(Qty, State, 0) ->
    apply(State#state.results_parser,
          [ apply(State#state.api_call,
                [State#state.'query',
                 State#state.args ++
                     [{count, Qty}]])
          ]
         );
%% If we do have a minimum ID from previous timeline/search
%% navigation, use that (minus 1) as the maximum ID we want Twitter to
%% give us the second time around
do_call(Qty, State, MinId) ->
    apply(State#state.results_parser,
          [ apply(State#state.api_call,
                [State#state.'query',
                 State#state.args ++
                     [{count, Qty},
                      {max_id, MinId - 1}]])
          ]
          ).

%% Utility function to take the latest results, merge them with the
%% existing results, and update state appropriately
process_latest(Latest, Accum, State) ->
    OldestId = find_id(lists:last(Latest)),
    { State#state{min_id=OldestId},
      lists:append(Accum,
                   lists:map(State#state.per_msg_fun, Latest)) }.

%%%%%% Test suite, should make eunit-compatible
test_timeline() ->
    ResultSet = timeline(user, {count, ?MAX_TL_REQ * 2 + 3}, fun(X) -> proplists:get_value(<<"id">>, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, ?MAX_TL_REQ * 2 + 3} = process_test_list(proplists:get_value(tweets, ResultSet)).

test_search() ->
    ResultSet = search("java", {count, ?MAX_SEARCH_REQ * 2 + 3}, fun(X) -> proplists:get_value(<<"id">>, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, ?MAX_SEARCH_REQ * 2 + 3} = process_test_list(proplists:get_value(tweets, ResultSet)).

process_test_list(List) ->
    ok = check_decrement(List),
    {lists:nth(1, List),
     lists:last(List),
     length(List)}.

check_decrement([H|T]) ->
    check_decrement(T, H).

%% Make sure the elements in a numerical list are ordered from highest to lowest
check_decrement([], _Prev) ->
    ok;
check_decrement([H|_T], Prev) when H > Prev ->
    fail;
check_decrement([_H|T], Prev) ->
    check_decrement(T, Prev).

%% Return the Twitter URL for the author of a tweet.  This is distinct
%% from any URL the author has supplied via his/her profile, which is
%% embedded in the tweet data itself
author_twitter_url(Tweet) ->
    RealTweet = extract_retweet(Tweet),
    io_lib:format("~ts", [[<<"https://twitter.com/">>,
                          proplists:get_value(<<"screen_name">>,
                                              proplists:get_value(<<"user">>, RealTweet))]]).

%% Take a tweet and determine its web link
%% https://twitter.com/hnycombinator/status/303131874795061248
tweet_url(Tweet) ->
    RealTweet = extract_retweet(Tweet),
    io_lib:format("~ts", [[author_twitter_url(RealTweet),
                          "/status/",
                          proplists:get_value(<<"id_str">>, RealTweet)]]).
