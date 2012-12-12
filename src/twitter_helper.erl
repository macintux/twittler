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
-export([timeline/3, extract_urls/1]).

%% @doc Take the specified timeline, return the specified number of
%% messages as processed by Fun
timeline(Which, {count, X}, Fun) when X >= 200 ->
    timeline(Which, X, 200, twitter_client:timeline(Which, [{count, 200}]), [], Fun);
timeline(Which, {count, X}, Fun) ->
    timeline(Which, X, X, twitter_client:timeline(Which, [{count, X}]), [], Fun).

%% Possible base cases:
%%   1  We've run out of messages to retrieve
%%        We'll know this happened when we receive fewer messages than we asked for on this run.  I think.
%%   2  We've retrieved as many messages as originally asked for
timeline(_Timeline, Remaining, Requested, Latest, Accum, Fun) when Requested > length(Latest) ->
    lists:reverse(lists:append(lists:map(Fun, Latest), Accum));
timeline(_Timeline, Remaining, _Requested, Latest, Accum, Fun) when Remaining - length(Latest) =< 0 ->
    lists:reverse(lists:append(lists:map(Fun, Latest), Accum));
timeline(Timeline, Remaining, Requested, Latest, Accum, Fun) ->
    OldestId = find_oldest_id(Latest),
    timeline(Timeline, Remaining - length(Latest), Requested,
             twitter_client:timeline(Timeline, [{count, Requested}, {max_id, OldestId - 1}]),
             lists:append(lists:map(Fun, Latest), Accum), Fun).

find_oldest_id([H | _T]) ->
    proplists:get_value(<<"id">>, H).


%% @doc Pull embedded URLs from a tweet
extract_urls(Tweet) ->
    Entities = proplists:get_value(<<"entities">>, Tweet),
    URLs = proplists:get_value(<<"urls">>, Entities),
    [ proplists:get_value(<<"url">>, X) || X <- URLs ].
