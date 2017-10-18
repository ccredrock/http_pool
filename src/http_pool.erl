%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <free>
%%% @doc
%%%         http interface && ba
%%% @end
%%% Created : 2017年07月05日19:11:34
%%%-------------------------------------------------------------------
-module(http_pool).

-export([start/1]).

-export([ba_post/6, http_post/5, http_post2/5,
         ba_put/6, http_put/5,
         ba_get/6, http_get/4,
         reply_ok/2,
         ba_auth/2, ba_auth/3, parse_client/1, auth_sign/2, auth_sign/3]).

%%------------------------------------------------------------------------------
%% @doc cowboy listen
%%------------------------------------------------------------------------------
start([{Name, Props} | T]) ->
    Dispatch = cowboy_router:compile([{'_', [{proplists:get_value(path, Props),
                                              proplists:get_value(handle, Props),
                                              [Name, Props]}]}]),
    {ok, _} = cowboy:start_http(erlang:make_ref(),
                                proplists:get_value(count, Props),
                                [{port, proplists:get_value(port, Props)}],
                                [{env, [{dispatch, Dispatch}]}]),
    start(T);
start([]) -> ok.

%%------------------------------------------------------------------------------
%% @doc http post
%%------------------------------------------------------------------------------
-spec ba_post(list(), integer(), list(), list(), list(), binary()) -> {ok, binary()} | {error, any()}.
ba_post(Host, Port, Client, Secret, Path, Body) ->
    Headers = do_make_ba_headers("POST", Client, Secret, Path),
    http_post(Host, Port, Headers, Path, Body).

http_post(Host, Port, Headers, Path, Body) ->
    Fun = fun(Pid) -> gun:post(Pid, Path, Headers, Body) end,
    do_http_exec(Host, Port, Fun, assert_body).

http_post2(Host, Port, Headers, Path, Body) ->
    Fun = fun(Pid) -> gun:post(Pid, Path, Headers, Body) end,
    do_http_exec(Host, Port, Fun, assert_info_head_body).

do_http_exec(Host, Port, Fun, Ret) ->
    case do_http_exec1(Host, Port, Fun, Ret) of
        {'EXIT', Reason} -> {error, Reason};
        Result -> Result
    end.

do_http_exec1(Host, Port, Fun, Ret) ->
    {ok, Pid} = gun:open(Host, Port, #{transport_opts => [{reuseaddr, true}]}),
    Info = gun:info(Pid),
    try
        {ok, http} = gun:await_up(Pid),
        Ref = Fun(Pid),
        case gun:await(Pid, Ref) of
            {response, fin, 200, _Headers} when Ret =:= assert_body ->
                {ok, <<>>};
            {response, nofin, 200, _Headers} when Ret =:= assert_body ->
                gun:await_body(Pid, Ref);
            {response, fin, 200, Headers} ->
                {Info, Headers, <<>>};
            {response, nofin, 200, Headers} ->
                {ok, Body} = gun:await_body(Pid, Ref),
                {Info, Headers, Body};
            {response, fin, Code, Headers} ->
                {error, {Code, Headers, <<>>}};
            {response, nofin, Code, Headers} ->
                {ok, Body} = gun:await_body(Pid, Ref),
                {error, {Code, Headers, Body}}
        end
    after
        gun:close(Pid),
        gun:flush(Pid)
    end.

%%------------------------------------------------------------------------------
%% @doc http get
%%------------------------------------------------------------------------------
ba_get(Host, Port, Client, Secret, Path, Query) ->
    Headers = do_make_ba_headers("GET", Client, Secret, Path),
    http_get(Host, Port, Headers, Path ++ "?" ++ Query).

http_get(Host, Port, Headers, Path) ->
    Fun = fun(Pid) -> gun:get(Pid, Path, Headers) end,
    do_http_exec(Host, Port, Fun, assert_body).

reply_ok(Body, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain;charset=utf-8">>}], Body, Req).

%%------------------------------------------------------------------------------
%% @doc http put
%%------------------------------------------------------------------------------
ba_put(Host, Port, Client, Secret, Path, Body) ->
    Headers = do_make_ba_headers("PUT", Client, Secret, Path),
    http_put(Host, Port, Headers, Path, Body).

http_put(Host, Port, Headers, Path, Body) ->
    Fun = fun(Pid) -> gun:put(Pid, Path, Headers, Body) end,
    do_http_exec(Host, Port, Fun, assert_body).

%%------------------------------------------------------------------------------
%% @doc ba auth
%%------------------------------------------------------------------------------
do_make_ba_headers(Method, Client, Secret, Path) ->
    Date = strftime:f(os:timestamp(), "%a, %d %b %Y %H:%M:%S GMT", universal),
    Auth = do_get_auth(Method, Client, Secret, Path, Date),
    [{<<"Content-Type">>, <<"application/json;charset=utf-8">>},
     {<<"Date">>, list_to_binary(Date)},
     {<<"Authorization">>, Auth}].

ba_auth(Req, SecretList) -> ba_auth("POST", Req, SecretList).
ba_auth(Method, Req, SecretList) ->
    Client = parse_client(Req),
    case lists:keyfind(Client, 1, SecretList) of
        false -> false;
        {_, Secret} -> auth_sign(Method, Req, Secret)
    end.

auth_sign(Req, Secret) -> auth_sign("POST", Req, Secret).
auth_sign(Method, Req, Secret) ->
    {Date, _} = cowboy_req:header(<<"date">>, Req),
    {Path, _} = cowboy_req:path(Req),
    do_get_sign(Method, Secret, Path, Date) =:= do_parse_sign(Req).

do_get_sign(Method, Secret, Path, Date) ->
    base64:encode(crypto:hmac(sha,
                              iolist_to_binary(Secret),
                              iolist_to_binary([Method, " ", Path, "\n", Date]))).

do_parse_sign(Req) ->
    {Auth, _}= cowboy_req:header(<<"authorization">>, Req),
    [<<"MWS ", _Client/binary>>, Sign] = re:split(Auth, ":"), Sign.

do_get_auth(Method, Client, Secret, Path, Date) ->
    Sign = do_get_sign(Method, Secret, Path, Date),
    iolist_to_binary(["MWS ", Client, ":", Sign]).

parse_client(Req) ->
    {Auth, _}= cowboy_req:header(<<"authorization">>, Req),
    [<<"MWS ", Client/binary>>, _Sign] = re:split(Auth, ":"), Client.

