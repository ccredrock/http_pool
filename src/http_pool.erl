%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <free>
%%% @doc
%%%         http接口封装
%%% @end
%%% Created : 2017年07月05日19:11:34
%%%-------------------------------------------------------------------
-module(http_pool).

-export([start/1]).

-export([ba_post/6, http_post/5, http_post2/5, ba_get/6,
         reply_ok/2,
         ba_auth/2, parse_client/1, auth_sign/2]).

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
-spec ba_post(list(), integer(), list(), list(), list(), binary()) -> {ok, binary()} | {error, any()}.
ba_post(Host, Port, Client, Secret, Path, Body) ->
    Headers = make_ba_headers(Client, Secret, Path),
    http_post(Host, Port, Headers, Path, Body).

make_ba_headers(Client, Secret, Path) ->
    Date = strftime:f(os:timestamp(), "%a, %d %b %Y %H:%M:%S GMT", universal),
    Auth = get_auth(Client, Secret, Path, Date),
    [{<<"Content-Type">>, <<"appliaction/json;charset=utf-8">>},
     {<<"Date">>, list_to_binary(Date)},
     {<<"Authorization">>, Auth}].

http_post(Host, Port, Headers, Path, Body) ->
    Fun = fun(Pid) -> gun:post(Pid, Path, Headers, Body) end,
    case http_exec(Host, Port, Fun) of
        {'EXIT', Reason} -> {error, Reason};
        Result -> Result
    end.

http_post2(Host, Port, Headers, Path, Body) ->
    Fun = fun(Pid) -> gun:post(Pid, Path, Headers, Body) end,
    case http_exec(Host, Port, Fun, 2) of
        {'EXIT', Reason} -> {error, Reason};
        Result -> Result
    end.

http_exec(Host, Port, Fun) ->
    http_exec(Host, Port, Fun, 0).

http_exec(Host, Port, Fun, Version) ->
    {ok, Pid} = gun:open(Host, Port),
    Info = gun:info(Pid),
    try
        {ok, http} = gun:await_up(Pid),
        Ref = Fun(Pid),
        case gun:await(Pid, Ref) of
            {response, fin, 200, _Headers} when Version =:= 0 ->
                {ok, <<>>};
            {response, nofin, 200, _Headers} when Version =:= 0 ->
                gun:await_body(Pid, Ref);
            {response, fin, 200, Headers} ->
                {Info, Headers, <<>>};
            {response, nofin, 200, Headers} ->
                {ok, Body} = gun:await_body(Pid, Ref),
                {Info, Headers, Body}
        end
    after
        gun:close(Pid),
        gun:flush(Pid)
    end.

http_get(Host, Port, Headers, Path) ->
    Fun = fun(Pid) -> gun:get(Pid, Path, Headers) end,
    case http_exec(Host, Port, Fun) of
        {'EXIT', Reason} -> {error, Reason};
        Result -> Result
    end.

-spec ba_get(list(), integer(), list(), list(), list(), list()) -> {ok, binary()} | {error, any()}.
ba_get(Host, Port, Client, Secret, Path, Query) ->
    Headers = make_ba_headers(Client, Secret, Path),
    http_get(Host, Port, Headers, Path ++ "?" ++ Query).

get_auth(Client, Secret, Path, Date) ->
    Sign = get_sign(Secret, Path, Date),
    iolist_to_binary(["MWS ", Client, ":", Sign]).

get_sign(Secret, Path, Date) ->
    base64:encode(crypto:hmac(sha,
                              iolist_to_binary(Secret),
                              iolist_to_binary(["POST ", Path, "\n", Date]))).

reply_ok(Body, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain;charset=utf-8">>}], Body, Req).

%%------------------------------------------------------------------------------
ba_auth(Req, SecretList) ->
    Client = parse_client(Req),
    case lists:keyfind(Client, 1, SecretList) of
        false -> false;
        {_, Secret} -> auth_sign(Req, Secret)
    end.

parse_client(Req) ->
    {Auth, _}= cowboy_req:header(<<"authorization">>, Req),
    [<<"MWS ", Client/binary>>, _Sign] = re:split(Auth, ":"), Client.

auth_sign(Req, Secret) ->
    {Date, _} = cowboy_req:header(<<"date">>, Req),
    {Path, _} = cowboy_req:path(Req),
    get_sign(Secret, Path, Date) =:= parse_sign(Req).

parse_sign(Req) ->
    {Auth, _}= cowboy_req:header(<<"authorization">>, Req),
    [<<"MWS ", _Client/binary>>, Sign] = re:split(Auth, ":"), Sign.

