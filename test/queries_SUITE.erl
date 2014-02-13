%% @hidden
-module(queries_SUITE).

-export([all/0, distance/1, rpc/1, filters/1, sorting/1, too_long/1, init_per_suite/1, end_per_suite/1]).
-export([rpc_return/2, rpc_echo/1, rpc_timeout/1, hsin/2]).

-include("lucene.hrl").

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() -> [distance, rpc, filters, too_long, sorting].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  lucene_server:start(),
  timer:sleep(2000),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec sorting(config()) -> _.
sorting(_Config) ->
  PageSize = 25,

  Sorter =
    fun
      ([F1, F2]) ->
        fun(A, B) ->
          proplists:get_value(F1, A) < proplists:get_value(F1, B)
          orelse
            (proplists:get_value(F1, A) == proplists:get_value(F1, B) andalso
              proplists:get_value(F2, A) =< proplists:get_value(F2, B))
        end;
      (F1) ->
        fun(A, B) ->
          proplists:get_value(F1, A) =< proplists:get_value(F1, B)
        end
    end,

  ok = lucene:clear(),
  Docs = [[{i, I}, {s, [$a + (I rem 5)]}] || I <- lists:seq(PageSize, 1, -1)],
  ok = lucene:add(Docs),

  SortedByI = lists:sort(Sorter(i), Docs),
  SortedByS = lists:sort(Sorter(s), Docs),
  SortedBySI = lists:sort(Sorter([s,i]), Docs),

  {Rs0, _} = lucene:match("i:[* TO *]", PageSize, [i]),
  SortedByI = [lists:keydelete('`score', 1, R) || R <- Rs0],

  {Rs1, _} = lucene:match("i:[* TO *]", PageSize, [s]),
  SortedByS = [lists:keydelete('`score', 1, R) || R <- Rs1],

  {Rs2, _} = lucene:match("i:[* TO *]", PageSize, [i,s]),
  SortedByI = [lists:keydelete('`score', 1, R) || R <- Rs2],

  {Rs3, _} = lucene:match("i:[* TO *]", PageSize, [s,i]),
  SortedBySI = [lists:keydelete('`score', 1, R) || R <- Rs3],

  {Rs4, _} = lucene:match("i:[* TO *]", PageSize, [{i, desc}]),
  SortedByI = lists:reverse([lists:keydelete('`score', 1, R) || R <- Rs4]),

  {Rs5, _} = lucene:match("i:[* TO *]", PageSize, [{s, desc}]),
  SortedBySI = lists:reverse([lists:keydelete('`score', 1, R) || R <- Rs5]),

  {Rs6, _} = lucene:match("i:[* TO *]", PageSize, [i, {s, desc}]),
  SortedByI = [lists:keydelete('`score', 1, R) || R <- Rs6],

  {Rs7, _} = lucene:match("i:[* TO *]", PageSize, [{i, desc}, {s, desc}]),
  SortedByI = lists:reverse([lists:keydelete('`score', 1, R) || R <- Rs7]),

  lucene:clear().

-spec too_long(config()) -> _.
too_long(_Config) ->
  PageSize = 5,

  ok = lucene:clear(),
  Docs = [[{x, x}]],
  ok = lucene:add(Docs),

  Query = "x:x OR x:" ++ [$a + I rem 20 || I <- lists:seq(1, 65535)],

  {_, M0} = lucene:match(Query, PageSize),
  1 = proplists:get_value(total_hits, M0),

  lucene:clear().

-spec filters(config()) -> _.
filters(_Config) ->
  PageSize = 5,

  ok = lucene:clear(),
  Docs = [[{x, x}, {i, integer_to_list(I-1)}, {s, [$a+I]}] || I <- lists:seq(PageSize*2, 1, -1)],
  ok = lucene:add(Docs),

  {_, M0} = lucene:match("x:x", PageSize, [], undefined),
  10 = proplists:get_value(total_hits, M0),
  {_, M1} = lucene:match("x:x", PageSize, [], []),
  10 = proplists:get_value(total_hits, M1),
  {_, M2} = lucene:match("x:x", PageSize, [], [{x, ["x"]}]),
  10 = proplists:get_value(total_hits, M2),
  {_, M3} = lucene:match("i:[1 TO 3]", PageSize, [], [{x, ["x"]}]),
  3 = proplists:get_value(total_hits, M3),
  {_, M4} = lucene:match("x:x", PageSize, [], [{s, ["b", "c", "d"]}]),
  3 = proplists:get_value(total_hits, M4),
  {_, M5} = lucene:match("x:x", PageSize, [], [{i, ["1", "2", "3"]}, {x, ["x", "y"]}]),
  3 = proplists:get_value(total_hits, M5),
  {_, M6} = lucene:match("x:x", PageSize, [], [{i, ["1", "2", "3"]}, {s, ["c"]}]),
  1 = proplists:get_value(total_hits, M6),

  {_, M7} = lucene:match("x:x", PageSize, [], [{i, ["1", "2", "3", "4", "5", "6"]}]),
  6 = proplists:get_value(total_hits, M7),
  Cont = proplists:get_value(next_page, M7),
  {[_], M8} = lucene:continue(Cont, PageSize),
  6 = proplists:get_value(total_hits, M8),
  6 = proplists:get_value(first_hit, M8),

  try lucene:match("x:x", PageSize, [], [{i, not_a_string}]) of
    R -> no_result = R
  catch
    _:_ -> ok
  end,

  lucene:clear().

-spec rpc_timeout([undefined | integer()]) -> term().
rpc_timeout(Is) -> timer:sleep(5000), rpc_echo(Is).

-spec rpc_return(term(), [undefined | integer()]) -> term().
rpc_return(Return, Is) -> [Return || _ <- Is].

-spec rpc_echo([undefined | integer()]) -> term().
rpc_echo(Is) -> [case I of undefined -> undefined; I -> erlang:float(I) end || I <- Is].

-spec rpc(config()) -> _.
rpc(_Config) ->
  PageSize = 5,

  ok = lucene:clear(),
  Docs = [[{i, I}, {s, [$a+I]}] || I <- lists:seq(PageSize, 1, -1)],
  ok = lucene:add(Docs),

  {[], _} = lucene:match("i.erlang:\"queries_SUITE:rpc_return:[false]\"", PageSize),
  {[], _} = lucene:match("s.erlang:\"queries_SUITE:rpc_return:[false]\"", PageSize),
  {Rs, M} = lucene:match("i.erlang:\"queries_SUITE:rpc_echo\"", PageSize),
  5 = proplists:get_value(total_hits, M),
  Docs = [lists:keydelete('`score', 1, R) || R <- Rs],

  {[], _} = lucene:match("i.erlang:\"queries_SUITE:rpc_timeout\"", PageSize),

  {[], _} = lucene:match("i.erlang:\"queries_SUITE:rpc_not_exported\"", PageSize),

  ALongName = [(I rem 10) + $a || I <- lists:seq(1,255)],
  {[], _} = lucene:match("i.erlang:\"" ++ ALongName ++ ":" ++ ALongName ++ "\"", PageSize),

  lucene:clear().

-spec distance(config()) -> _.
distance(_Config) ->
  PageSize = 5,

  try lucene:add([[{g, #geo{lat=-91.0}}]]) of
    R0 -> throw = R0
  catch
    _:LatErr -> {invalid_latitude, -91.0} = LatErr
  end,

  try lucene:add([[{g, #geo{lat=1.0, lng=181.0}}]]) of
    R1 -> throw = R1
  catch
    _:LngErr -> {invalid_longitude, 181.0} = LngErr
  end,

  DsSouth = [[{g, #geo{lat=1.0 * I, lng=0.0}},   {d, s}, {i, I}] || I <- lists:seq(1, PageSize)],
  DsNorth = [[{g, #geo{lat=-1.0 * I, lng=0.0}},  {d, n}, {i, I}] || I <- lists:seq(1, PageSize)],
  DsEast  = [[{g, #geo{lat=0.0, lng=1.0 * I}},   {d, e}, {i, I}] || I <- lists:seq(1, PageSize)],
  DsWest  = [[{g, #geo{lat=0.0, lng=-1.0 * I}},  {d, w}, {i, I}] || I <- lists:seq(1, PageSize)],

  ok = lucene:add(DsSouth),
  ok = lucene:add(DsNorth),
  ok = lucene:add(DsWest),
  ok = lucene:add(DsEast),

  wait_for_docs(),

  _ = lager:info("south"),
  {JustSouth, _} = lucene:match("d:s AND g.near:0.0,0.0,10000", PageSize),
  true = is_sorted(distances(#geo{lat=0.0, lng=0.0}, JustSouth)),
  _ = lager:info("north"),
  {JustNorth, _} = lucene:match("d:n AND g.near:0.0,0.0,10000", PageSize),
  true = is_sorted(distances(#geo{lat=0.0, lng=0.0}, JustNorth)),

  _ = lager:info("0.0"),
  {All, _} = lucene:match("g.near:0.0,0.0,10000", PageSize*4),
  true = is_sorted(distances(#geo{lat=0.0, lng=0.0}, All)),

  _ = random:seed(erlang:now()),
  Lat = random_latitude(),
  Lng = random_longitude(),
  _ = lager:info("~f,~f", [Lat,Lng]),
  {AllRnd, _} = lucene:match(io_lib:format("g.near:~f,~f,10000", [Lat, Lng]), PageSize*4),
  true = is_sorted(distances(#geo{lat=Lat, lng=Lng}, AllRnd)),
  ok.

distances(Origin, Docs) ->
  [hsin(Origin, proplists:get_value(g, Doc)) || Doc <- Docs].

is_sorted([]) -> true;
is_sorted([_]) -> true;
is_sorted([X,Y|Rest]) when X =< Y -> is_sorted([Y|Rest]);
is_sorted([X,Y|_]) ->
  _ = lager:info("~p > ~p: ~p", [X, Y, X - Y]), false.

-spec hsin(lucene:geo(), lucene:geo()) -> float().
hsin(P1, P2) ->
  DLat = math:pi() * (P2#geo.lat - P1#geo.lat) / 180,
  DLng = math:pi() * (P2#geo.lng - P1#geo.lng) / 180,
  Lat1 = math:pi() * P1#geo.lat / 180,
  Lat2 = math:pi() * P2#geo.lat / 180,
  A = math:sin(DLat/2) * math:sin(DLat/2) + math:sin(DLng/2) * math:sin(DLng/2) * math:cos(Lat1) * math:cos(Lat2),
  erlang:round(2 * math:atan2(math:sqrt(A), math:sqrt(1-A)) * 3959 * 10000) / 10000.

random_longitude() -> (random:uniform(10000000) - 16500000) / 100000.
random_latitude() -> (random:uniform(3000000) + 2100000) / 100000.

wait_for_docs() -> wait_for_docs(10).
wait_for_docs(0) -> throw(tired_of_waiting);
wait_for_docs(I) ->
  try lucene:match("d:[* TO *]", 1)
  catch
    _:_ ->
      receive
        after 1000 ->
          _ = lager:info("Waiting ~p more times", [I]),
          wait_for_docs(I-1)
      end
  end.
