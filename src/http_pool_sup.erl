%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <meituan>
%%% @doc
%%%
%%% @end
%%% Created : 2017年07月07日12:11:03
%%%-------------------------------------------------------------------
-module(http_pool_sup).

-export([start_link/0]).
-export([init/1]).

%%------------------------------------------------------------------------------
-behaviour(supervisor).

%%------------------------------------------------------------------------------
start_link() ->
    {ok, List} = application:get_env(http_pool, pools, []),
    http_pool:start(List),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 60}, []}}.

