-module(http_pool_tests).

-include_lib("eunit/include/eunit.hrl").

%% callback
-export([init/3, handle/2, terminate/3]).

-define(Setup, fun() -> application:start(http_pool) end).
-define(Clearnup, fun(_) -> application:stop(http_pool) end).

basic_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [{"ba",
        fun() ->
                X = http_pool:ba_post("127.0.0.1", 8080, "client1", "asdasdffsadf", "/test/aa", <<"null">>),
                timer:sleep(1000),
                io:format(user, "post result ~p ~n", [X])
        end}]
     }
    }.

%%------------------------------------------------------------------------------
init({_, http}, Req, [_, Callback]) ->
    {ok, Req, Callback}.

handle(Req, State) ->
    case http_pool:ba_auth(Req, [{<<"client1">>, <<"asdasdffsadf">>}]) of
        true ->
            http_pool:reply_ok(<<"">>, Req),
            {ok, Req, State};
        false ->
            http_pool:reply_ok(<<"123">>, Req),
            {ok, Req, State}
    end.

terminate(_, _, _) ->
    ok.

