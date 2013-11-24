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
-export([timeline/3, timeline/4, extract_urls/1, extract_mentions/1, extract_retweet/1, author_details/1, search/3]).
-export([follow/1]).

-export([oembed_tweets/2, is_retweet/1]).

-export([list_network/2]).

-export([run_tests/0, test_timelines/0, test_search/0]).

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
          max_per_request, %% Twitter's max request counter for this type of
                           %% request (see -defines above)
          max_id=0, %% First message we encounter
          min_id=0, %% Last message we encounter
          args=[] %% Arguments to pass, not including count, max_id
         }).

-type state() :: #state{}.

%% Account management foo


%% Utility function to invoke from strip_at
grab_re(Orig, nomatch) ->
    Orig;
grab_re(_Orig, {match, List}) ->
    lists:nth(1, List).

strip_at(Screenname) ->
    grab_re(Screenname, re:run(Screenname, "^\s*@(.*)\s*", [{capture, all_but_first, list}])).

%% For moment, assumes list of @<usernames>
follow(List) ->
    lists:foreach(fun(X) -> io:format("~p~n", [twittler:follow({screen_name, strip_at(X)})]) end,
                  List).

%% @doc Helper function to check for the presence of next_results in
%% the search_metadata (or next_cursor in friends/followers) in the
%% return value from the latest set of results
%%
%% Note: next_results often doesn't appear in search results, so not
%% using this for searching.
-spec check_for_cursor(term()) -> boolean().
check_for_cursor(undefined) ->
    false;
check_for_cursor(_) ->
    true.

%% Who: friends or followers
%% Args: Twitter API parameters as list of key/value tuples
list_network(Who, Args) ->
    list_network(Who, Args, -1).

list_network(Who, Args, Cursor) ->
    chase_network(Who, Args, twittler:network(Who, Args ++ [{cursor, Cursor}]), []).

chase_network(Who, Args, Results, Accum) ->
    keep_chasing_network(Who, Args, proplists:get_value(next_cursor, Results),
                         proplists:get_value(users, Results, []), Accum).

keep_chasing_network(_Who, _Args, _Cursor, [], Accum) ->
    Accum;
keep_chasing_network(_Who, _Args, undefined, Users, Accum) ->
    Accum ++ Users;
keep_chasing_network(Who, Args, Cursor, Users, Accum) ->
    chase_network(Who, Args,
                  twittler:network(Who, Args ++ [{cursor, Cursor}]), Accum ++ Users).

%% @doc Take the specified search, return the specified number of
%% messages as processed by Fun
search(Query, {count, X}, Fun) ->
    {State, List} =
        unified(X,
                #state{'query' = Query,
                       api_call = fun twittler:search/2,
                       max_per_request = ?MAX_SEARCH_REQ,
                       results_parser = fun(Qty, Y) ->
                                                Tweets =
                                                    proplists:get_value(statuses, Y),
                                                { Qty >= length(Tweets) andalso length(Tweets) > 0,
                                                  Tweets
                                                }
                                        end,
                       per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ].


timeline(user, {screen_name, Who}, {count, X}, Fun) ->
    {State, List} =
        unified(X,
                #state{'query' = user,
                       args = [{screen_name, Who}],
                       max_per_request = ?MAX_TL_REQ,
                       api_call = fun twittler:timeline/2,
                       results_parser = fun(Qty, Y) ->
                                                { Qty >= length(Y) andalso length(Y) > 0,
                                                  Y
                                                }
                                        end,
                       per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ].

timeline(Which, {count, X}, Fun) ->
    {State, List} =
        unified(X,
                #state{'query' = Which,
                       api_call = fun twittler:timeline/2,
                       max_per_request = ?MAX_TL_REQ,
                       results_parser = fun(Qty, Y) ->
                                                { Qty >= length(Y) andalso length(Y) > 0,
                                                  Y
                                                }
                                        end,
                       per_msg_fun = Fun}),
    [ {max_id, State#state.max_id}, {min_id, State#state.min_id}, {tweets, List} ].


find_id(Tweet) ->
    proplists:get_value(id, Tweet).

%% @doc Pull embedded URLs from a tweet. Always check for retweets
extract_urls(Tweet) ->
    Entities = proplists:get_value(entities, extract_retweet(Tweet)),
    URLs = proplists:get_value(urls, Entities),
    [ proplists:get_value(url, X) || X <- URLs ].

%% @doc Pull mentions from a tweet. Always check for retweets
%% Returns a list of {Name, ScreenName} pairs
extract_mentions(Tweet) ->
    Entities = proplists:get_value(entities, extract_retweet(Tweet)),
    Mentions = proplists:get_value(user_mentions, Entities),
    [ {proplists:get_value(name, X), proplists:get_value(screen_name, X)} || X <- Mentions ].

%% @doc Pull author details from a tweet. Always check for retweets
%% Returns { Name, ScreenName, Description, URL }
author_details(Tweet) ->
    User = proplists:get_value(user, extract_retweet(Tweet)),
    { proplists:get_value(name, User), proplists:get_value(screen_name, User),
      proplists:get_value(description, User), proplists:get_value(url, User) }.

%% @doc Pull embedded native retweet if present
extract_retweet(Tweet) ->
    extract_retweet(Tweet, proplists:get_value(retweeted_status, Tweet)).

extract_retweet(Tweet, undefined) ->
    Tweet;
extract_retweet(_Tweet, Retweet) ->
    Retweet.

is_retweet(Tweet) ->
    proplists:get_value(retweeted_status, Tweet) =/= undefined.


%% Find the first ID in a list of tweets
find_first_id([]) ->
    0;
find_first_id(Tweets) ->
    find_id(lists:nth(1, Tweets)).

%% Consolidate timeline, search
%% unified() returns { State, List }

%% Entry point
unified(HowMany, State) ->
    unified(true, HowMany, [], [], State).

%% First invocation of unified/5
unified(true, Remaining, [], [], State) ->
    {Continue, Tweets} =
        do_call(min(State#state.max_per_request, Remaining), State),
    FirstId = find_first_id(Tweets),
    unified(Continue, Remaining - length(Tweets), Tweets, [],
            State#state{max_id=FirstId});

%% One ending point: we've gotten all that the client asked for
unified(true, Remaining, Latest, Accum, State) when Remaining =< 0 ->
    process_latest(Latest, Accum, State);

%% Other ending point: the results_parser function told us to stop
unified(false, _Remaining, Latest, Accum, State) ->
    process_latest(Latest, Accum, State);

%% Intermediate invocation
unified(true, Remaining, Latest, Accum, State) ->
    {NewState, NewAccum} = process_latest(Latest, Accum, State),
    {Continue, Tweets} =
        do_call(min(State#state.max_per_request, Remaining),
                NewState),
    unified(Continue,
            Remaining - length(Tweets),
            Tweets,
            NewAccum, NewState).

-spec do_call(integer(), state()) -> { boolean(), list(list()) }.
do_call(NextQty, State) ->
    do_call(NextQty, State, State#state.min_id).

%% If we don't have a minimum ID yet, that means we start at the first
%% tweet Twitter will give us.
do_call(Qty, State, 0) ->
    Results = apply(State#state.api_call,
                    [State#state.'query',
                     State#state.args ++ [{count, Qty}]]),
    %% Watch for invalid user accounts in timeline requests
    case Results of
        {wrong_url, _Errmsg} ->
            { false, [] };
        _ ->
            apply(State#state.results_parser,
                  [ Qty,
                    Results
                  ]
                 )
    end;
%% If we do have a minimum ID from previous timeline/search
%% navigation, use that (minus 1) as the maximum ID we want Twitter to
%% give us the second time around
do_call(Qty, State, MinId) ->
    apply(State#state.results_parser,
          [ Qty,
            apply(State#state.api_call,
                  [State#state.'query',
                   State#state.args ++
                       [{count, Qty},
                        {max_id, MinId - 1}]])
          ]
          ).

%% Utility function to take the latest results, merge them with the
%% existing results, and update state appropriately.
%%
%% When we hit the end of search results or a timeline, the first
%% argument will be an empty list.
process_latest([], Accum, State) ->
    { State, Accum };
process_latest(Latest, Accum, State) ->
    OldestId = find_id(lists:last(Latest)),
    { State#state{min_id=OldestId},
      lists:append(Accum,
                   lists:map(State#state.per_msg_fun, Latest)) }.

%%%%%% Test suite, should make eunit-compatible
run_tests() ->
    test_timelines(),
    test_search().

test_timelines() ->
    test_timeline(),
    test_timeline2(),
    test_timeline3().

test_timeline() ->
    ResultSet = timeline(user, {count, ?MAX_TL_REQ * 2 + 3}, fun(X) -> proplists:get_value(id, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, ?MAX_TL_REQ * 2 + 3} = process_test_list(proplists:get_value(tweets, ResultSet)).

test_timeline2() ->
    ResultSet = timeline(user, {count, ?MAX_TL_REQ}, fun(X) -> proplists:get_value(id, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, ?MAX_TL_REQ} = process_test_list(proplists:get_value(tweets, ResultSet)).

test_timeline3() ->
    ResultSet = timeline(user, {count, ?MAX_TL_REQ-3}, fun(X) -> proplists:get_value(id, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, ?MAX_TL_REQ-3} = process_test_list(proplists:get_value(tweets, ResultSet)).

test_search() ->
    test_search_more(),
    test_search_toofew(),
    test_search_none().

test_search_more() ->
    ResultSet = search("java", {count, ?MAX_SEARCH_REQ * 2 + 3}, fun(X) -> proplists:get_value(id, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, ?MAX_SEARCH_REQ * 2 + 3} = process_test_list(proplists:get_value(tweets, ResultSet)).

test_search_none() ->
    ResultSet = search("aecfioué#!", {count, ?MAX_SEARCH_REQ * 2 + 3}, fun(X) -> proplists:get_value(id, X) end),
    0 = proplists:get_value(max_id, ResultSet),
    0 = proplists:get_value(min_id, ResultSet),
    [] = proplists:get_value(tweets, ResultSet).

test_search_toofew() ->
    ResultSet = search("gist.github.com/macintux", {count, ?MAX_SEARCH_REQ * 2 + 3}, fun(X) -> proplists:get_value(id, X) end),
    MaxId = proplists:get_value(max_id, ResultSet),
    MinId = proplists:get_value(min_id, ResultSet),
    {MaxId, MinId, _Count} = process_test_list(proplists:get_value(tweets, ResultSet)).

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

oembed_header(FH) ->
    io:fwrite(FH, "<html>~n<head><meta charset='utf-8'></head><body><script src='http://platform.twitter.com/widgets.js' charset='utf-8'></script>~n", []).

oembed_tweets(Tweets, Filename) ->
    {ok, FH} = file:open(Filename, [write, {encoding, utf8}]),
    oembed_header(FH),
    lists:foreach(
      fun(X) -> io:fwrite(FH, "~ts~n",
                          [ case is_retweet(X) of
                                true ->
                                    "";
                                false ->
                                    proplists:get_value(html,
                                                        twittler:status(
                                                          binary_to_list(
                                                            proplists:get_value(id_str, X)),
                                                          oembed))
                            end
                          ]
                         )
      end,
      Tweets),
    io:fwrite(FH, "</body></html>~n", []),
    file:close(FH).
