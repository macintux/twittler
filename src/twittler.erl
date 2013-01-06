%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Twitter 1.1 API wrapper.
%%% @end

%% Missing:
%% * DM handling
%% * Any POST requests
%% * User/list/follower/friendship/notification functions
%% * 3-legged OAuth
%% * Streaming

-module(twittler).
-behavior(gen_server).

%% Our API
-export([dev_auth/4, pin_auth/2, pin_auth/4, start/1, timeline/2,
         status/1, status/2, stop/0, search/1, search/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(auth, {
          ckey="",
          csecret="",
          atoken="",
          asecret="",
          method=hmac_sha1
          }).

-type auth() :: #auth{}.

-record(state, {
          auth,
          user,
          urls=[]
          }).

-type state() :: #state{}.

-define(BASE_URL(X), "http://api.twitter.com/1.1/" ++ X).
-define(OAUTH_URL(X), "https://api.twitter.com/oauth/" ++ X).

-define(SERVER, ?MODULE).

-type timeline() :: 'home' | 'user' | 'mentions'.

%% API

%% @doc Construct an authorization object with developer access token
%%
%% @spec start(ConsumerKey::string(), ConsumerSecret::string(),
%%             AccessToken::string(), AccessTokenSecret::string()) -> Auth::auth()
-spec dev_auth(string(), string(), string(), string()) -> auth().
dev_auth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret) ->
    inets:start(),
    ssl:start(),
    #auth{ckey=ConsumerKey,
          csecret=ConsumerSecret,
          atoken=AccessToken,
          asecret=AccessTokenSecret}.

%% @doc Retrieve URL that will allow user to get PIN to proceed.
%%      Return value will be { Url, Token }, where the Url should be
%%      given to the user to generate a Twitter PIN, and the token
%%      should be the third argument passed to pin_auth/4
%%
%% @see pin_auth/4
-spec pin_auth(string(), string()) -> { string(), string() }.
pin_auth(ConsumerKey, ConsumerSecret) ->
    inets:start(),
    ssl:start(),
    OAuthToken =
        request_url(post,
                    { url, ?OAUTH_URL("request_token"), [ { 'oauth_callback', 'oob' } ] },
                    #auth{ckey=ConsumerKey, csecret=ConsumerSecret},
                    fun(Body) -> proplists:get_value("oauth_token",
                                                     oauth:uri_params_decode(Body))
                    end),
    { oauth:uri(?OAUTH_URL("authenticate"),
                [ { 'oauth_token', OAuthToken } ]),
      OAuthToken }.

%% @doc Leverage user PIN to retrieve access token. Returns Auth
%%      object.  Beyond the app key/secret, takes the oauth_token as
%%      retrieved from the first step and the user-provided PIN as
%%      arguments.
%%
%%      IMPORTANT: It is necessary that the PIN be passed as a list
%%      (string), not an integer, if it is prefaced with a 0.
%%
%% @see pin_auth/2
-spec pin_auth(string(), string(), string(), string()) -> auth().
pin_auth(ConsumerKey, ConsumerSecret, RequestToken, PIN) ->
    request_url(post,
                { url, ?OAUTH_URL("access_token"),
                  [ { oauth_verifier, PIN },
                    { oauth_token, RequestToken} ]
                },
                #auth{ckey=ConsumerKey, csecret=ConsumerSecret},
                fun(Body) ->
                        Proplist = oauth:uri_params_decode(Body),
                        #auth{ckey=ConsumerKey, csecret=ConsumerSecret,
                              atoken=proplists:get_value("oauth_token",
                                                         Proplist),
                              asecret=proplists:get_value("oauth_token_secret",
                                                          Proplist)}
                end).



%% @doc Starts the API service
%%
%% @spec start(Auth::auth()) -> {ok, Pid::pid()}

-spec start(auth()) -> {ok, pid()}.
start(Auth) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Auth], []).

%% @doc Fetch the specified timeline with optional arguments per the Twitter API

-spec timeline(timeline(), list()) -> any().
timeline(What, Args) ->
    gen_server:call(?SERVER, {timeline, What, Args}).


status(Id) ->
    gen_server:call(?SERVER, {status, show, Id}).

status(Id, retweets) ->
    gen_server:call(?SERVER, {status, retweets, Id});
status(Id, oembed) ->
    gen_server:call(?SERVER, {status, oembed, Id}).

search(Query) ->
    gen_server:call(?SERVER, {search, Query, []}).

search(Query, Args) ->
    gen_server:call(?SERVER, {search, Query, Args}).

stop() ->
    gen_server:cast(?SERVER, stop).


%% behavior implementation
init([Auth]) ->
    Urls = twitter_urls(),
    init_auth(Auth, Urls, twitter_call(#state{auth=Auth, urls=Urls}, verify_creds, [])).

init_auth(_Auth, _Urls, {_Error, Message}) ->
    {stop, Message};
init_auth(Auth, Urls, UserData) ->
    {ok, #state{auth=Auth, urls=Urls, user=UserData}}.

handle_call({timeline, What, Args}, _From, State) ->
    {reply, twitter_call(State, list_to_atom(atom_to_list(What) ++ "_timeline"), Args), State};
handle_call(whoami, _From, State) ->
    {reply, State#state.user, State};
handle_call({status, What, Id}, _From, State) ->
    {reply, twitter_call(State, list_to_atom("status_" ++ atom_to_list(What)), [ { id, Id } ]), State};
handle_call({search, Query, Args}, _From, State) ->
    {reply, twitter_call(State, search, [{ q, Query }] ++ Args), State}.



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

-spec twitter_call(state(), atom(), list()) -> any().
twitter_call(State, What, UrlArgs) ->
    UrlDetails = proplists:get_value(What, State#state.urls),
    request_url(UrlDetails#url.method,
                {url, UrlDetails#url.url, UrlArgs},
                State#state.auth,
                fun(X) -> parse_statuses(X) end
               ).

-spec twitter_urls() -> list(url()).
twitter_urls() ->
    [ { home_timeline, #url{url=?BASE_URL("statuses/home_timeline.json")} },
      { user_timeline, #url{url=?BASE_URL("statuses/user_timeline.json")} },
      { mentions_timeline, #url{url=?BASE_URL("statuses/mentions_timeline.json")} },
      { retweets_of_me_timeline, #url{url=?BASE_URL("statuses/retweets_of_me.json")} },

      { verify_creds, #url{url=?BASE_URL("account/verify_credentials.json"), result=user} },

      { status_show, #url{url=?BASE_URL("statuses/show.json")} },
      { status_retweets, #url{url=?BASE_URL("statuses/retweets.json")} },
      { status_oembed, #url{url=?BASE_URL("statuse/oembed.json")} },
      { search, #url{url=?BASE_URL("search/tweets.json")} }
    ].

-spec request_url('get'|'post', {url, string(), list()}, auth(), fun()) -> any().
request_url(HttpMethod, {url, Url, UrlArgs}, #auth{ckey=ConsumerKey, csecret=ConsumerSecret, method=Method, atoken=AccessToken, asecret=AccessSecret}, Fun) ->
    check_http_results(apply(oauth, HttpMethod, [Url, UrlArgs, {ConsumerKey, ConsumerSecret, Method}, AccessToken, AccessSecret]), Fun).

-spec check_http_results(tuple(), fun()) -> any().
check_http_results({ok, {{_HttpVersion, 200, _StatusMsg}, _Headers, Body}}, Fun) ->
    Fun(Body);
check_http_results({ok, {{_HttpVersion, 401, StatusMsg}, _Headers, Body}}, _Fun) ->
    {oauth_error, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 403, StatusMsg}, _Headers, Body}}, _Fun) ->
    {forbidden, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 404, StatusMsg}, _Headers, Body}}, _Fun) ->
    {wrong_url, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 406, StatusMsg}, _Headers, Body}}, _Fun) ->
    {wrong_param, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 413, StatusMsg}, _Headers, Body}}, _Fun) ->
    {too_long_param, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 416, StatusMsg}, _Headers, Body}}, _Fun) ->
    {bad_range, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 420, StatusMsg}, _Headers, Body}}, _Fun) ->
    {retry, rate_limited, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 429, StatusMsg}, _Headers, Body}}, _Fun) ->
    {retry, rate_limited, extract_error_message(StatusMsg, Body) };
check_http_results({ok, {{_HttpVersion, 503, StatusMsg}, _Headers, Body}}, _Fun) ->
    {retry, unavailable, extract_error_message(StatusMsg, Body) };
check_http_results(Other, _Fun) ->
    {unknown, Other}.


%% Currently, Twitter provides a body message like:
%%   { "errors": [ { "message":"Could not auth", "code":32 } ] }
%%
%% Unclear why the object is buried in a list.
%%
%% Rather than assume that structure will remain constant, attempt to
%% decode the JSON, but return the HTTP status header if it fails.
-spec extract_error_message(string(), string()) -> string().
extract_error_message(HttpStatusMsg, Body) ->
    try
        Contents = jsx:decode(list_to_binary(Body)),
        [H | _T] = proplists:get_value(list_to_binary("errors"), Contents),
        binary_to_list(proplists:get_value(list_to_binary("message"),
                                           H))
    catch
        _:_ ->
            HttpStatusMsg
    end.


-spec parse_statuses(binary() | string()) -> any().
parse_statuses(JSON) when is_binary(JSON) ->
    jsx:decode(JSON);
parse_statuses(JSON) ->
    jsx:decode(list_to_binary(JSON)).
