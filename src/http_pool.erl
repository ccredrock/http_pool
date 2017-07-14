%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <meituan>
%%% @doc
%%%
%%% @end
%%% Created : 2017年07月05日19:11:34
%%%-------------------------------------------------------------------
-module(http_pool).

-export([start/1]).

%% callback
-export([init/3, handle/2, terminate/3]).

-export([ba_post/6, ba_auth/2, parse_client/1, auth_sign/2]).

%%------------------------------------------------------------------------------
-behaviour(cowboy_http_handler).

%%------------------------------------------------------------------------------
-callback handle_req(DeadNode::binary()) -> ok | {error, Reason::term()}.
-callback get_secret(DeadNode::binary()) -> ok | {error, Reason::term()}.

%%------------------------------------------------------------------------------
start([{_Name, Props} | T]) ->
    Dispatch = cowboy_router:compile([{'_', [{proplists:get_value(path, Props),
                                              ?MODULE,
                                              [proplists:get_value(callback, Props)]}]}]),
    cowboy:start_http(?MODULE,
                      proplists:get_value(count, Props),
                      [{port, proplists:get_value(port, Props)}],
                      [{env, [{dispatch, Dispatch}]}]),
    start(T);
start([]) -> ok.

%%------------------------------------------------------------------------------
init({_, http}, Req, [Callback]) ->
    {ok, Req, Callback}.

handle(Req, {Mod, Fun} = Callback) ->
    case catch Mod:Fun(Req) of
        {ok, Ret} -> cowboy_req:reply(200, [], Ret, Req);
        {error, Ret} -> cowboy_req:reply(400, [], Ret, Req);
        {error, Code, Ret} -> cowboy_req:reply(iolist_to_binary(Code), [], Ret, Req);
        {'EXIT', Reason} -> error_logger:error_msg("http_pool error ~p~n", [{Reason}])
    end,
    {ok, Req, Callback}.

terminate(_, _, _) ->
    ok.

%%------------------------------------------------------------------------------
ba_post(Host, Port, Client, Secret, Path, Payload) ->
    Date = strftime:f(os:timestamp(), "%a, %d %b %Y %H:%M:%S GMT", universal),
    Auth = get_auth(Client, Secret, Path, Date),
    {ok, Pid} = gun:open(Host, Port),
    try
        Ref = gun:post(Pid, Path,
                       [{<<"content-type">>, <<"appliaction/json">>},
                        {<<"Date">>, list_to_binary(Date)},
                        {<<"Authorization">>, Auth}], Payload),
        {ok, http} = gun:await_up(Pid),
        {response, _, 200, _Headers} = gun:await(Pid, Ref), ok
    catch E:R -> {false, {E,R, erlang:get_stacktrace()}}
    after
        gun:close(Pid),
        gun:flush(Pid)
    end.

get_auth(Client, Secret, Path, Date) ->
    Sign = get_sign(Secret, Path, Date),
    iolist_to_binary(["MWS ", Client, ":", Sign]).

get_sign(Secret, Path, Date) ->
    base64:encode(crypto:hmac(sha,
                              iolist_to_binary(Secret),
                              iolist_to_binary(["POST ", Path, "\n", Date]))).

%%------------------------------------------------------------------------------
ba_auth(Req, SecretList) ->
    Client = parse_client(Req),
    case lists:keyfind(Client, 1, SecretList) of
        false -> false;
        {_, Secret} -> auth_sign(Req, Secret)
    end.

parse_client(Req) ->
    {Auth, _Req} = cowboy_req:header(<<"authorization">>, Req),
    [<<"MWS ", Client/binary>>, _Sign] = re:split(Auth, ":"), Client.

auth_sign(Req, Secret) ->
    {Date, _} = cowboy_req:header(<<"date">>, Req),
    {Path, _} = cowboy_req:path(Req),
    get_sign(Secret, Path, Date) =:= parse_sign(Req).

parse_sign(Req) ->
    {Auth, _Req} = cowboy_req:header(<<"authorization">>, Req),
    [<<"MWS ", _Client/binary>>, Sign] = re:split(Auth, ":"), Sign.

