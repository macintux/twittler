%%% -*- mode:erlang -*-
{application, twittler, [
    {description, "Erlang Twitter API"},
    {vsn, "0.2"},
    {modules, [twittler, twitter_helper]},
    {registered, []},
    {applications, [kernel, stdlib, inets, oauth]},
    {env, []}
]}.
