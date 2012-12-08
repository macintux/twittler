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
-behavior(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.5").

%% Our API
-export([build_auth/4, build_auth/2, start/1, timeline/2, status/1, stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(auth, {
          ckey,
          csecret,
          atoken,
          asecret,
          method=hmac_sha1
          }).

-type auth() :: #auth{}.

-record(state, {
          auth,
          user,
          urls=[]
          }).

%% Twitter objects
-include("twitter_client.hrl").

-define(BASE_URL(X), "http://api.twitter.com/1.1/" ++ X).
-define(SERVER, ?MODULE).

-type timeline() :: 'home' | 'user' | 'mentions'.

%% API

%% @doc Construct an authorization object
%%
%% @spec start(ConsumerKey::string(), ConsumerSecret::string(),
%%             AccessToken::string(), AccessTokenSecret::string()) -> Auth::auth()
build_auth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret) ->
    #auth{ckey=ConsumerKey,
          csecret=ConsumerSecret,
          atoken=AccessToken,
          asecret=AccessTokenSecret}.

%% Will eventually handle 3-legged oauth
build_auth(ConsumerKey, ConsumerSecret) ->
    enotimplemented.

%% @doc Starts the API service
%%
%% @spec start(Auth::auth()) -> {ok, Pid::pid()}

start(Auth) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Auth], []).

%% @doc Fetch the specified timeline with optional arguments per the Twitter API

timeline(What, Args) ->
    gen_server:call(?SERVER, {timeline, What, Args}).


status(Id) ->
    gen_server:call(?SERVER, {status, Id}).

stop() ->
    gen_server:cast(?SERVER, stop).

%% behavior implementation
init([Auth]) ->
    Urls = twitter_urls(),
    %% call account/verify_credentials to get id, screen_name, name
    User = twitter_call(#state{auth=Auth, urls=Urls}, verify_creds, []),
    {ok, #state{auth=Auth, urls=Urls, user=User}}.

handle_call({timeline, What, Args}, _From, State) ->
    %% XXX : specifying home_timeline here is a temporary hack for testing
    {reply, twitter_call(State, home_timeline, Args), State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_X, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

-record(url, {
          url,
          method=get,
          result=status
          }).

-type url() :: #url{}.



twitter_call(State, What, UrlArgs) ->
    UrlDetails = proplists:get_value(What, State#state.urls),
    request_url(UrlDetails#url.method, UrlDetails#url.url, State#state.auth,
                UrlArgs, fun(X) -> parse_statuses(X, UrlDetails#url.result) end).

twitter_urls() ->
    [ { home_timeline, #url{url=?BASE_URL("statuses/home_timeline.json")} },
      { user_timeline, #url{url=?BASE_URL("statuses/user_timeline.json")} },
      { mentions_timeline, #url{url=?BASE_URL("statuses/mentions_timeline.json")} },
      { verify_creds, #url{url=?BASE_URL("account/verify_credentials.json"), result=user} },
      { status, #url{url=?BASE_URL("statuses/show.json")} }
    ].

request_url(get, Url, #auth{ckey=ConsumerKey, csecret=ConsumerSecret, method=Method, atoken=AccessToken, asecret=AccessSecret}, Args, Fun) ->
    case oauth:get(Url, Args, {ConsumerKey, ConsumerSecret, Method}, AccessToken, AccessSecret) of
        {ok, {_, _, "Failed to validate oauth signature or token"}} -> {oauth_error, "Failed to validate oauth signature or token"};
        {ok, {_, _, Body}} -> Fun(Body);
        Other -> Other
    end.

parse_statuses(JSON, ResultType) ->
    filter_results(jsx:decode(list_to_binary(JSON)), ResultType).

%% Sometimes Twitter gives us a list of statuses (list of proplists),
%% sometimes a single status (proplist). Examine the head of the
%% (formerly JSON, now parsed) list, see if it is a tuple
filter_results([H | T], Type) when is_tuple(H) ->
    recify([H|T], Type);
filter_results(Nodes, Type) ->
    [recify(Node, Type) || Node <- Nodes].

recify(Node, user) ->
    user_rec(Node);
recify(Node, status) ->
    status_rec(Node).


%%    [status_rec(Node) || Node <- jsx:decode(list_to_binary(JSON)) ].

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
