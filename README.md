Erlang Twitter API library.

## Status

This is still very primitive (needs supervision, many functionality
gaps remain, zero documentation) but is usable today.

## Dependencies

* OAuth: [erlang-oauth](https://github.com/tim/erlang-oauth)
* JSON: [jsx](https://github.com/talentdeficit/jsx)

## Building

Nothing fancy here yet. Build `jsx` and `erlang-oauth`, copy their
beam files into this project's `ebin/` folder, and run `make` to build
`twittler` itself.

## Running, illustrated

<div class="info">
The examples below are out of date.  The most notable change is that
the `jsx` library has been instructed to convert proplist keys to atoms
instead of leaving them as binary. This is a bit of a security risk for
any long-running application, because it's possible to exhaust the
virtual machine's supply of atoms, but is fine for futzing around.

I have a plan to address this when it becomes important: see
https://github.com/talentdeficit/jsx/pull/37
</div>


There is no support for unauthenticated API calls so you'll need a
consumer key and secret, which can be obtained free of charge from
https://dev.twitter.com/ by starting an application.

You can use an access token from dev.twitter.com or PIN-based
authentication. I do not yet have any support for 3-legged OAuth for
web apps.

For this example, I'll use `erl` and PIN-based auth.

    $ erl -pa ebin
    Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.9.1  (abort with ^G)
    2> { Url, Token } = twittler:pin_auth("<redacted consumer key>", "<redacted consumer secret>").
    {"https://api.twitter.com/oauth/authenticate?oauth_token=W5Fqdfo4geaucPpwwVu3imxFhTDGurPbGiwbvmLY0c",
     "W5Fqdfo4geaucPpwwVu3imxFhTDGurPbGiwbvmLY0c"}
    3> Url.
    "https://api.twitter.com/oauth/authenticate?oauth_token=W5Fqdfo4geaucPpwwVu3imxFhTDGurPbGiwbvmLY0c"

At this point, I pasted that URL into my browser, authenticate, and
received a numeric token. *Note*: Twitter occasionally returns a PIN
that starts with 0, which Erlang will happily (and incorrectly) treat
as octal. Always quote the PIN when passing it to the second
invocation of `pin_auth`.

    5> Auth = twittler:pin_auth("<redacted consumer key>", "<redacted consumer secret>", Token, "8214454").
    {auth,"<redacted consumer key>",
          "<redacted consumer secret>"
          "800713560-JrbYRYKvCnvHuN7mbP0OwdkA8KATkOp2ZyQZKMRl",
          "BYnCOiArIAWZZWDEqE7RIKH6VpSVfjrd87GF5q0",hmac_sha1}
    6> twittler:start(Auth).
    {ok,<0.71.0>}

`start/1` will fire up a process to maintain state. There is no
mechanism in place yet to restart it if it fails.

    7> twittler:timeline(home, []).
    [[{<<"created_at">>,<<"Fri Jan 04 02:16:24 +0000 2013">>},
      {<<"id">>,287019620316102657},
      {<<"id_str">>,<<"287019620316102657">>},
      {<<"text">>,
       <<"0-2: respect. 3-4: pitty. 5+: contempt. My feeling towards people under 40 based on how "...>>},
      {<<"source">>,<<"web">>},
      {<<"truncated">>,false},
      {<<"in_reply_to_status_id">>,null},
      {<<"in_reply_to_status_id_str">>,null},
      {<<"in_reply_to_user_id">>,null},
      {<<"in_reply_to_user_id_str">>,null},
      {<<"in_reply_to_screen_name">>,null},
      {<<"user">>,
       [{<<"id">>,34876390},
        {<<"id_str">>,<<"34876390">>},
        {<<"name">>,<<"Mahmud X Other">>},
        {<<"screen_name">>,<<"bigthingist">>},
        {<<"location">>,<<"Hunter S Thompson Valley, NS"...>>},
        {<<"description">>,<<"Abstractional Polemicist"...>>},
        {<<"url">>,<<"http://scholar.googl"...>>},
        {<<"entities">>,[{<<"url">>,[{...}]},{<<"desc"...>>,[...]}]},
        {<<"protected">>,false},
        {<<"followers_co"...>>,767},
        {<<"friends_"...>>,594},
        {<<"list"...>>,61},
        {<<...>>,...},
        {...}|...]},
      {<<"geo">>,null},
      {<<"coordinates">>,null},
      {<<"place">>,null},
      {<<"contributors">>,null},
      {<<"retweet_count">>,0},
      {<<"entities">>,
       [{<<"hashtags">>,[]},
        {<<"urls">>,[]},
        {<<"user_mentions">>,[]}]},
      {<<"favorited">>,false},
      {<<"retweeted">>,false}],
    ...

    8> te_helper:timeline(home, {count, 5}, fun(X) -> io_lib:format("~s", [proplists:get_value(<<"text">>, X)]) end).
    [{max_id,287019620316102657},
     {min_id,287014532348534784},
     {tweets,[["0-2: respect. 3-4: pitty. 5+: contempt. My feeling towards people under 40 based on how many children they have."],
              ["private eyes"],
              ["Figured out stripe and paypal express checkout. Going to revamp my purchase process using that instead of wepay."],
              ["Going to work at 2PM after a night of research. My work today next few days: investigate OpenCV, research machines and file formats. Yes!"],
              ["RT @built: Fellow programmers, this is the most beautiful thing you will see today: https://t.co/OiwFbn0P  @munificentbob you are specia ..."]]}]

As you can see, the core `twittler` module provides (more or less) raw
access to the Twitter API, while the `te_helper` module offers
functions to simplify certain operations.  In particular, `te_helper`
can intelligently navigate a timeline or list of search results that
exceed the native Twitter limits.

The last tweet above is a native retweet, which means there's a nested
object with the full, non-truncated text. By pure luck I have its
Twitter ID (the `min_id` value, returned with any timeline or search),
so I can grab the embedded tweet:

    12> io:format("~s~n", [proplists:get_value(<<"text">>, te_helper:extract_retweet(twittler:status(287014532348534784)))]).
    Fellow programmers, this is the most beautiful thing you will see today: https://t.co/OiwFbn0P  @munificentbob you are specially gifted. :)

If I didn't happen to have the ID, I could always re-run the timeline call, grabbing `<<"id">>` along with `<<"text">>`.

The `te_helper` module also has an `extract_urls` function:

    14> io:format("~p~n", [te_helper:extract_urls(twittler:status(287014532348534784))]).
    [<<"https://t.co/OiwFbn0P">>]

If the tweet returned by `twittler:status` is a native retweet,
`te_helper` will grab the nested tweet before extracting the URLs,
because URLs are occasionally truncated otherwise.


Want to see everyone who has recently sent a tweet with Erlang and the
`#code2012` hash tag? Of course you do.

    23> te_helper:search("#code2012 erlang", {count, 5}, fun(X) -> io:format("~ts (@~s)~n", [proplists:get_value(<<"name">>, proplists:get_value(<<"user">>, X)), proplists:get_value(<<"screen_name">>, proplists:get_value(<<"user">>, X))]) end).
    Freddy (@gUstehn)
    Björn-Egil Dahlberg (@psyeugenic)
    Martin Wiso (@tajgur)
    Dr. Andrew F Ledvina (@wolfgangFabian)
    Erlanger (@remonde1986)
    [{max_id,287549052034375680},
     {min_id,287110826383523841},
     {tweets,[ok,ok,ok,ok,ok]}]


## History

This library originated with Nick Gerakines
(https://github.com/ngerakines/erlang_twitter) and my intent was to
patch his code, but with the significant changes to Twitter's API,
mandatory OAuth, and my unwillingness to spend lots of time managing
records to represent Twitter objects it was necessary to effectively
replace all of the Erlang.

A few build files remain, like `Makefile` and `include.mk`.
