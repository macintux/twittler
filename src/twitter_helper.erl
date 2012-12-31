%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%    Utilities for smarter Twitter API handling. The core library is
%%%    closer to the raw Twitter API.
%%%
%%%    The core Twitter library must already be initialized.
%%% @end
%%% Created : 11 Dec 2012 by John Daily <jd@epep.us>

-module(twitter_helper).
-export([timeline/3, extract_urls/1, extract_retweet/1, search/3]).

-define(MAX_TL_REQ, 200).
-define(MAX_SEARCH_REQ, 100).

%% A search is effectively same as a timeline, but the results aren't structured the same.
%% Results: [{"statuses", [<list of tweets>]}, {search_metadata, [<list_of_metadata>]}]

%% We could make the timeline() function more general by supplying
%% parser functions and the API function to invoke for each iteration.



%% @doc Take the specified search, return the specified number of
%% messages as processed by Fun
search(Query, {count, X}, Fun) when X > ?MAX_SEARCH_REQ ->
    search(Query, X - ?MAX_SEARCH_REQ, ?MAX_SEARCH_REQ,
           twitter_client:search(Query, [{count, ?MAX_SEARCH_REQ}, {result_type, "recent"}]), [], Fun);
search(Query, {count, X}, Fun) ->
    search(Query, 0, X,
           twitter_client:search(Query, [{count, X}, {result_type, "recent"}]), [], Fun).

%% Possible base cases:
%%   1  We've run out of messages to retrieve
%%        We'll know this happened when we receive fewer messages than we asked for on this run.  I think.
%%   2  We've retrieved as many messages as originally asked for
search(_Query, _Remaining, Requested, [{<<"statuses">>, Latest}, {<<"search_metadata">>, _MD}], Accum, Fun) when Requested > length(Latest) ->
    lists:reverse(lists:append(lists:map(Fun, Latest), Accum));
search(_Query, Remaining, _Requested, [{<<"statuses">>, Latest}, {<<"search_metadata">>, _MD}], Accum, Fun) when Remaining =< 0 ->
    lists:reverse(lists:append(lists:map(Fun, Latest), Accum));
search(Query, Remaining, Requested, [{<<"statuses">>, Latest}, {<<"search_metadata">>, _MD}], Accum, Fun) when Remaining < Requested ->
    OldestId = find_id(lists:last(Latest)),
    ToRequest = Remaining,
    search(Query, 0, ToRequest,
             twitter_client:search(Query, [{count, ToRequest}, {max_id, OldestId - 1}, {result_type, "recent"}]),
             lists:append(lists:map(Fun, Latest), Accum), Fun);
search(Query, Remaining, Requested, [{<<"statuses">>, Latest}, {<<"search_metadata">>, _MD}], Accum, Fun) ->
    OldestId = find_id(lists:last(Latest)),
    ToRequest = Requested,
    search(Query, Remaining - ToRequest, ToRequest,
             twitter_client:search(Query, [{count, ToRequest}, {max_id, OldestId - 1}, {result_type, "recent"}]),
             lists:append(lists:map(Fun, Latest), Accum), Fun).


%% @doc Take the specified timeline, return the specified number of
%% messages as processed by Fun
timeline(Which, {count, X}, Fun) when X > ?MAX_TL_REQ ->
    timeline(Which, X - ?MAX_TL_REQ, ?MAX_TL_REQ, twitter_client:timeline(Which, [{count, ?MAX_TL_REQ}]), [], Fun);
timeline(Which, {count, X}, Fun) ->
    timeline(Which, 0, X, twitter_client:timeline(Which, [{count, X}]), [], Fun).

%% Possible base cases:
%%   1  We've run out of messages to retrieve
%%        We'll know this happened when we receive fewer messages than we asked for on this run.  I think.
%%   2  We've retrieved as many messages as originally asked for
timeline(_Timeline, _Remaining, Requested, Latest, Accum, Fun) when Requested > length(Latest) ->
    lists:reverse(lists:append(lists:map(Fun, Latest), Accum));
timeline(_Timeline, Remaining, _Requested, Latest, Accum, Fun) when Remaining =< 0 ->
    lists:reverse(lists:append(lists:map(Fun, Latest), Accum));
timeline(Timeline, Remaining, Requested, Latest, Accum, Fun) when Remaining < Requested ->
    OldestId = find_id(lists:last(Latest)),
    ToRequest = Remaining,
    timeline(Timeline, 0, ToRequest,
             twitter_client:timeline(Timeline, [{count, ToRequest}, {max_id, OldestId - 1}]),
             lists:append(lists:map(Fun, Latest), Accum), Fun);
timeline(Timeline, Remaining, Requested, Latest, Accum, Fun) ->
    OldestId = find_id(lists:last(Latest)),
    ToRequest = Requested,
    timeline(Timeline, Remaining - ToRequest, ToRequest,
             twitter_client:timeline(Timeline, [{count, ToRequest}, {max_id, OldestId - 1}]),
             lists:append(lists:map(Fun, Latest), Accum), Fun).

find_id(Tweet) ->
    proplists:get_value(<<"id">>, Tweet).

%% @doc Pull embedded URLs from a tweet. Always check for retweets
extract_urls(Tweet) ->
    Entities = proplists:get_value(<<"entities">>, extract_retweet(Tweet)),
    URLs = proplists:get_value(<<"urls">>, Entities),
    [ proplists:get_value(<<"url">>, X) || X <- URLs ].

%% @doc Pull embedded native retweet if present
extract_retweet(Tweet) ->
    extract_retweet(Tweet, proplists:get_value(<<"retweeted_status">>, Tweet)).

extract_retweet(Tweet, undefined) ->
    Tweet;
extract_retweet(_Tweet, Retweet) ->
    Retweet.
