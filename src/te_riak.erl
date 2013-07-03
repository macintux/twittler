%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Store Meetup data in Riak
%%% @end
%%% Created : 8 Mar 2013 by John Daily <jd@epep.us>

-module(te_riak).

-compile(export_all).

stream_tweets(R) ->
    receive
        {message, M} ->
            store_tweet(M, R);
        X ->
            io:format("Got message ~p, discarding~n", [X])
    end,
    stream_tweets(R).


stream_tweets(_R, 0) ->
    done;
stream_tweets(R, Count) ->
    receive
        {message, M} ->
            store_tweet(M, R);
        X ->
            io:format("Got message ~p, discarding~n", [X])
    end,
    stream_tweets(R, Count-1).


store_tweet(Tweet, R) ->
    Bucket = <<"tweets">>,
    Key = proplists:get_value(id_str, Tweet),
    JSON = tweet_to_storage(Tweet),
    Object = create_object(Bucket, Key, JSON),
    MDObject = add_indexes(Tweet, Object),
    put_object(add_indexes(Tweet, MDObject), R).

tweet_to_storage(Tweet) ->
    jsx:encode([{name, proplists:get_value(name,
                                           proplists:get_value(user, Tweet))},
                {screen_name, proplists:get_value(screen_name,
                                                  proplists:get_value(user, Tweet))},
                {message, proplists:get_value(text, Tweet)},
                {favorite_count, proplists:get_value(favorite_count, Tweet)},
                {retweet_count, proplists:get_value(retweet_count, Tweet)}
               ]).

add_indexes(Tweet, Object) ->
    riakc_obj:update_metadata(Object,
                              create_metadata(indexes(Tweet),
                                              riakc_obj:get_update_metadata(Object))).

indexes(Tweet) ->
    [
     {{integer_index, "id"}, [proplists:get_value(id, Tweet)]},
     {{binary_index, "screen_name"}, [proplists:get_value(screen_name,
                                                          proplists:get_value(user, Tweet))]},
     {{binary_index, "hashtags"}, pull_hashtags(Tweet)},
     {{binary_index, "created"}, [proplists:get_value(created_at, Tweet)]}
    ].

pull_hashtags(Tweet) ->
    lists:map(fun(X) -> proplists:get_value(text, X) end,
              proplists:get_value(hashtags,
                                  proplists:get_value(entities, Tweet))).


%% Each index gets added and a new object returned for the next index
create_metadata([], MetaData) ->
    MetaData;
create_metadata([H|T], MetaData) ->
    create_metadata(T, riakc_obj:add_secondary_index(MetaData, H)).


put_object(Object, R) ->
    riakc_pb_socket:put(R, Object).

retrieve_or_create_object(Bucket, Key, R) ->
    create_object_unless(riakc_pb_socket:get(R, Bucket, Key),
                         Bucket, Key).

create_object_unless({ok, Obj}, _Bucket, _Key) ->
    Obj;
create_object_unless({error, _}, Bucket, Key) ->
    create_object(Bucket, Key).

create_object(Bucket, Key) ->
    riakc_obj:new(Bucket, Key).

create_object(Bucket, Key, Value) ->
    riakc_obj:new(Bucket, Key, Value).
