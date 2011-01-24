-module(cbuf_server).

-behaviour(gen_server).

-compile(export_all).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {table, length, name, pos = 0}).

start_link() -> start_link(512).

start_link(Length) ->
  gen_server:start_link(?MODULE, [Length], []).

start_link(Length, Name) ->
  gen_server:start_link(?MODULE, [Length, Name], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([Length]) ->
  init([Length, <<>>]);
init([Length, Name]) ->
  TableID = ets:new(my_little_cbuf, [ordered_set]),
  % We have to pre-make our slots so we don't hit $end_of_table atoms.
  [ets:insert(TableID, [{N, now(), <<>>}]) || N <- lists:seq(0, Length)],
  {ok, #state{table = TableID, length = Length,
              name = Name, pos = 0}}.

handle_call({add, Data}, _From, #state{table = Tid,
                                       length = Len, pos = Pos} = State) ->
  LocalPos = Pos rem Len,
  ets:insert(Tid, {LocalPos, now(), Data}),
  {reply, ok, State#state{pos = Pos + 1}};

handle_call(position, _From, #state{pos = Position} = State) ->
  {reply, Position, State};

handle_call({back, N}, _From, #state{length = Len, pos = CurrentPos} = State) ->
  Diff = abs(CurrentPos - N),
  io:format("CurrentPos, N, Len: ~p, ~p, ~p~n", [CurrentPos, N, Len]),
  if
    Diff > Len -> handle_call(all_entries, nil, State);
          true -> handle_call({entries, N}, nil, State)
  end;

handle_call(all_entries, _From, #state{table = Tid,
                                        length = Len, pos = P} = State) ->
  {reply, back_n_entries(Len, Tid, Len, P), State};

handle_call({entries, N}, _From, #state{table = Tid,
                                        length = Len, pos = P} = State) ->
  {reply, back_n_entries(N, Tid, Len, P), State};

handle_call(dump_table, _From, #state{table = Tid} = State) ->
  {reply, ets:tab2list(Tid), State}.


handle_cast(_Request, State) ->
  {noreply, State}.

terminate(_Reason, #state{table = Tid}) ->
  io:format("Table is: ~p~n", [ets:tab2list(Tid)]),
  ok.

handle_info(Info, State) ->
  io:format("Other info of: ~p~n", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%----------------------------------------------------------------------
%%% Internal Functions
%%%----------------------------------------------------------------------
back_n_entries(N, _, Length, _) when N > Length -> [];
back_n_entries(N, Tid, Length, Position) when
  is_integer(N), is_integer(Length), is_integer(Position) ->
  LiveDataPosition = Position rem Length,
  LocalPos = if
               N > LiveDataPosition -> Length - (N - LiveDataPosition);
                               true -> LiveDataPosition - N
             end,
  {Position, get_back_n_entries(N, Tid, Length, LocalPos, [])}.

get_back_n_entries(0, _, _, _, Accum) ->
  lists:reverse(Accum);
get_back_n_entries(N, Tid, Length, LocalPos, Accum) when LocalPos =:= Length ->
  get_back_n_entries(N, Tid, Length, 0, Accum);  % WRAP
get_back_n_entries(N, Tid, Length, LocalPos, Accum) ->
  [{_Idx, _Now, Data}] = ets:slot(Tid, LocalPos),
  get_back_n_entries(N - 1, Tid, Length, LocalPos + 1, [Data|Accum]).

%%%----------------------------------------------------------------------
%%% Testing
%%%----------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
cbuf_test() ->
  cbuf:test().
-endif.
