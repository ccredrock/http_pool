%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <meituan>
%%% @doc
%%%
%%% @end
%%% Created : 2017年07月07日12:10:04
%%%-------------------------------------------------------------------
-module(http_pool_app).

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
-behaviour(application).

%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ssl:start(),
    application:ensure_started(cowlib),
    application:ensure_started(ranch),
    application:ensure_started(gun),
    application:ensure_started(cowboy),
    http_pool_sup:start_link().

stop(_State) ->
    ok.
