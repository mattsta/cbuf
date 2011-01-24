-module(cbuf).

-compile(export_all).

start_link() -> cbuf_server:start_link(512).

start_link(Length) ->
  cbuf_server:start_link(Length).

start_link(Length, Name) ->
  cbuf_server:start_link(Length, Name).

%%%----------------------------------------------------------------------
%%% Usable Functions
%%%----------------------------------------------------------------------

add(Server, Data) ->
  gen_server:call(Server, {add, Data}).

entries(Server, Count) ->
  gen_server:call(Server, {back, Count}).

dump_table(Server) ->
  gen_server:call(Server, dump_table).

position(Server) ->
  gen_server:call(Server, position).

all_entries(Server) ->
  gen_server:call(Server, all_entries).

%%%----------------------------------------------------------------------
%%% Testing
%%%----------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
cbuf_test() ->
  {ok, I} = start_link(5),
  ?assertEqual({0, [<<>>, <<>>, <<>>, <<>>, <<>>]}, entries(I, 45)),
  ?assertEqual(ok, add(I, <<"hello1">>)),
  ?assertEqual({1, [<<>>, <<>>, <<>>, <<>>, <<"hello1">>]}, entries(I, 45)),
  ?assertEqual(ok, add(I, <<"hello2">>)),
  ?assertEqual(ok, add(I, <<"hello3">>)),
  ?assertEqual(ok, add(I, <<"hello4">>)),
  ?assertEqual(ok, add(I, <<"hello5">>)),
  ?assertEqual({5, [<<"hello5">>]}, entries(I, 1)),
  ?assertEqual({5, [<<"hello4">>, <<"hello5">>]}, entries(I, 2)),
  ?assertEqual({5, [<<"hello3">>, <<"hello4">>, <<"hello5">>]}, entries(I, 3)),
  ?assertEqual({5, [<<"hello2">>, <<"hello3">>, <<"hello4">>, <<"hello5">>]},
    entries(I, 4)),
  ?assertEqual({5, [<<"hello1">>, <<"hello2">>, <<"hello3">>, <<"hello4">>,
    <<"hello5">>]}, entries(I, 5)),
  dump_table(I),
  ?assertEqual(ok, add(I, <<"hello6">>)),
  dump_table(I),
  entries(I, 4500),
  ?assertEqual({6, [<<"hello2">>, <<"hello3">>, <<"hello4">>, <<"hello5">>,
    <<"hello6">>]}, entries(I, 5)),
  ?assertEqual({6, [<<"hello2">>, <<"hello3">>, <<"hello4">>, <<"hello5">>,
    <<"hello6">>]}, entries(I, 500)).
-endif.
