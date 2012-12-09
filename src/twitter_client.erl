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
    {reply, twitter_call(State, list_to_atom(atom_to_list(What) ++ "_timeline"), Args), State};
handle_call(whoami, _From, State) ->
    {reply, State#state.user, State};
handle_call({status, Id}, _From, State) ->
    {reply, twitter_call(State, status, [ { id, Id } ]), State}.



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

parse_statuses(JSON, _Type) ->
    jsx:decode(list_to_binary(JSON)).
