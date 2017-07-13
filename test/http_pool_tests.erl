-module(http_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-export([handle/1]).

-define(Setup, fun() -> application:start(http_pool) end).
-define(Clearnup, fun(_) -> application:stop(http_pool) end).

basic_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [{"ba",
        fun() ->
                X = http_pool:ba_post("127.0.0.1", 8080, "client1", "asdasdffsadf", "/test", <<"null">>),
                timer:sleep(1000),
                io:format(user, "post result ~p ~n", [X])
        end}]
     }
    }.

handle(Req) ->
    case http_pool:ba_auth(Req, [{<<"client1">>, <<"asdasdffsadf">>}]) of
        true -> {ok, <<>>};
        false -> {error, 1000, <<>>}
    end.

