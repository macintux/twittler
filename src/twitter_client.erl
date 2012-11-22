%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008-2009 Nick Gerakines
%% @version 0.4
%% @doc Provides access to the Twitter web service. Mostly through the
%% clever use of REST-like requests and XML parsing.
%%
%% This module attempts to provide complete access to the Twitter API. In
%% addition, it provides a simple gen_server process template to allow calls
%% to be made on behalf of a named user without having to send a username
%% and password each time.
%%
%% When the gen_server feature is used to make Twitter API calls for a user,
%% a gen_server process is spawned locally and its name is prefixed to
%% prevent named process collision.
%%
%% <strong>Make sure you start inets (<code>inets:start().</code>) before you do
%% anything.</strong>
%%
%% <h4>Quick start</h4>
%% <pre><code>
%% 1&gt; inets:start().
%% 2&gt; twitter_client:start("myname", "pass").
%% 3&gt; twitter_client:account_verify_credentials("myname", "pass", []).
%%   OR
%% 3&gt; twitter_client:call("myname", account_verify_credentials).
%% 4&gt; twitter_client:call("myname", user_timeline).
%% 5&gt; twitter_client:call("myname", status_update, [{"status", "Testing the erlang_twitter twitter_client.erl library."}]).
%% 6&gt; twitter_client:call("myname", user_timeline).
%% </code></pre>

%% Updates, late 2012: converting to the 1.1 Twitter API
%%
%% Dropped for the moment:
%% * DM handling
%% * Any POST requests
%% * Bulk retrieval of any kind, including timelines
%% * User/list/follower/friendship/notification functions
%%
%% Not yet added:
%% * Streaming
%% * 3-legged OAuth
%%
%% Need to review the header file, make sure we drop any obsolete
%% record fields.
-module(twitter_client).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.5").

-export([
    status_home_timeline/2,
    status_user_timeline/2,
    status_mentions/2,
    status_show/2,
    favorites/2,
    headers/2,
    parse_status/1, parse_statuses/1,
    request_url/5,
    build_url/2
]).

-include("twitter_client.hrl").

-define(BASE_URL(X), "http://api.twitter.com/1.1/" ++ X).

%% https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline
status_home_timeline(Auth, Args) when is_tuple(Auth), is_list(Args) ->
    Url = build_url("statuses/home_timeline.json", []),
    request_url(get, Url, Auth, Args, fun(X) -> parse_statuses(X) end).

%% https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
status_user_timeline(Auth, Args) ->
    Url = build_url("statuses/user_timeline.json", []),
    request_url(get, Url, Auth, Args, fun(X) -> parse_statuses(X) end).

%% https://dev.twitter.com/docs/api/1.1/get/statuses/mentions_timeline
status_mentions(Auth, Args) ->
    Url = build_url("statuses/mentions_timeline.json", []),
    request_url(get, Url, Auth, Args, fun(X) -> parse_statuses(X) end).

%% https://dev.twitter.com/docs/api/1.1/get/statuses/show/%3Aid
status_show(Auth, Args) ->
    Url = build_url("statuses/show.json", []),
    request_url(get, Url, Auth, Args, fun(X) -> parse_status(X) end).

favorites(Auth, Args) ->
    Url = build_url("favorites/list.json", []),
    request_url(get, Url, Auth, Args, fun(X) -> parse_statuses(X) end).


build_url(Url, []) -> Url;
build_url(Url, Args) ->
    Url ++ "?" ++ lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end, [],
            [K ++ "=" ++ twitter_client_utils:url_encode(V) || {K, V} <- Args]
        )
    ).

request_url(get, Url, {Consumer, Token, Secret}, Args, Fun) ->
    case oauth:get(?BASE_URL(Url), Args, Consumer, Token, Secret) of
        {ok, {_, _, "Failed to validate oauth signature or token"}} -> {oauth_error, "Failed to validate oauth signature or token"};
        {ok, {_, _, Body}} -> Fun(Body);
        Other -> Other
    end.

headers(nil, nil) -> [{"User-Agent", "ErlangTwitterClient/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) ->
    Basic = "Basic " ++ binary_to_list(base64:encode(User ++ ":" ++ Pass)),
    [{"User-Agent", "ErlangTwitterClient/0.1"}, {"Authorization", Basic}, {"Host", "twitter.com"}].

parse_statuses(JSON) ->
    [status_rec(Node) || Node <- jsx:decode(list_to_binary(JSON)) ].

parse_status(JSON) ->
    status_rec(jsx:decode(list_to_binary(JSON))).

status_rec(undefined) ->
    undefined;
status_rec(Node) ->
    #status{
        created_at = proplists:get_value(<<"created_at">>, Node),
        id = proplists:get_value(<<"id">>, Node),
        text = proplists:get_value(<<"text">>, Node),
        source = proplists:get_value(<<"source">>, Node),
        truncated = proplists:get_value(<<"truncated">>, Node),
        in_reply_to_status_id = proplists:get_value(<<"in_reply_to_status_id">>, Node),
        in_reply_to_user_id = proplists:get_value(<<"in_reply_to_status_id">>, Node),
        favorited = proplists:get_value(<<"favorited">>, Node),
        %% For direct messages, this is "sender", for a status message it's "user"
        user = user_rec(proplists:get_value(<<"sender">>, Node, proplists:get_value(<<"user">>, Node)))
    }.

user_rec(undefined) ->
    undefined;
user_rec(Node) ->
    #user{
        id = proplists:get_value(<<"id">>, Node),
        name = proplists:get_value(<<"name">>, Node),
        screen_name = proplists:get_value(<<"screen_name">>, Node),
        location = proplists:get_value(<<"location">>, Node),
        description = proplists:get_value(<<"description">>, Node),
        %% profile_image_url = text_or_default(Node, ["/user/profile_image_url/text()", "/sender/profile_image_url/text()"], ""),
        %% url = text_or_default(Node, ["/user/url/text()", "/sender/url/text()"], ""),
        %% protected = text_or_default(Node, ["/user/protected/text()", "/sender/protected/text()"], ""),
        %% followers_count = text_or_default(Node, ["/user/followers_count/text()", "/sender/followers_count/text()"], ""),
        %% profile_background_color = text_or_default(Node, ["/user/profile_background_color/text()"], ""),
        %% profile_text_color = text_or_default(Node, ["/user/profile_text_color/text()"], ""),
        %% profile_link_color = text_or_default(Node, ["/user/profile_link_color/text()"], ""),
        %% profile_sidebar_fill_color = text_or_default(Node, ["/user/profile_sidebar_fill_color/text()"], ""),
        %% profile_sidebar_border_color = text_or_default(Node, ["/user/profile_sidebar_border_color/text()"], ""),
        %% friends_count = text_or_default(Node, ["/user/friends_count/text()"], ""),
        %% created_at = text_or_default(Node, ["/user/created_at/text()"], ""),
        %% favourites_count = text_or_default(Node, ["/user/favourites_count/text()"], ""),
        %% utc_offset = text_or_default(Node, ["/user/utc_offset/text()"], ""),
        %% time_zone = text_or_default(Node, ["/user/time_zone/text()"], ""),
        %% following = text_or_default(Node, ["/user/following/text()"], ""),
        %% notifications = text_or_default(Node, ["/user/notifications/text()"], ""),
        %% statuses_count = text_or_default(Node, ["/user/statuses_count/text()"], ""),
        status = status_rec(proplists:get_value(<<"status">>, Node))
    }.

%% Occasionally useful for troubleshooting
%%
%% echo_body([H | Body]) when H =:= $< ->
%%     io:format("<~s~n", [ Body ]);
%% echo_body(JSON) ->
%%     jsx:prettify(list_to_binary(JSON)).
