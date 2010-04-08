-module(splaytest).

-export([test_sets/0, test_dict/0]).

-compile(export_all).

test_sets() ->
    Els = random_els(20),
    {_,_} = statistics(runtime),
    Set0 = test_sets_add(Els, splaysets:new()),
    {_,AddTime} = statistics(runtime),
    io:format("From ~p els, Set0 has ~p els~n", [ length(Els), splaysets:size(Set0)]),
    {_,_} = statistics(runtime),
    io:format("Test Fetch returned: ~p~n", [test_sets_fetch(Els, Set0)]),
    {_,FetchTime} = statistics(runtime),
    Set1 = test_sets_remove(Els, Set0),
    {_,RemoveTime} = statistics(runtime),
    io:format("Removed from set. Set now ~p~n", [Set1]),
    io:format("Add took ~p ms. Fetching took ~p ms. Removal took ~p ms.~n", [AddTime, FetchTime, RemoveTime]),
    ok.

test_sets_add([], Set) -> Set;
test_sets_add([E|Es], Set) ->
    test_sets_add(Es, splaysets:add_element(E, Set)).

test_sets_remove([], Set) -> Set;
test_sets_remove([E|Es], Set) ->
    test_sets_remove(Es, splaysets:del_element(E, Set)).

show_set_height(Set, N) ->
    case N > 0 andalso N rem 5 =:= 0 of
        true -> io:format("~nWith ~p els, height should be lg(~p)=~p. Actual: ~p~n~p~n~n",
                          [ N, N, round(math:log(N)), splaysets:height(Set), Set]);
        _ -> ignore
    end.

test_sets_fetch([], _) -> ok;
test_sets_fetch([E|Es], Set) ->
    case splaysets:is_element(E, Set) of
        true -> test_sets_fetch(Es, Set);
        false -> erlang:error({missing_element, E})
    end.

test_dict() ->
    Els = random_kvs(100),
    {_,_} = statistics(runtime),
    Dict0 = test_dict_add(Els, splaydict:new()),
    {_,AddTime} = statistics(runtime),
    io:format("From ~p els, Dict0 has ~p els~n", [ length(Els), splaydict:size(Dict0)]),
    {_,_} = statistics(runtime),
    io:format("Test Fetch returned: ~p~n", [test_dict_fetch(Els, Dict0)]),
    {_,FetchTime} = statistics(runtime),
    Dict1 = test_dict_remove(Els, Dict0),
    {_,RemoveTime} = statistics(runtime),
    io:format("Removed from dict. Dict now ~p~n", [Dict1]),
    io:format("Add took ~p ms. Fetching took ~p ms. Removal took ~p ms.~n", [AddTime, FetchTime, RemoveTime]),
    ok.

test_dict_add([], D) -> show_dict_height(D, splaydict:size(D)), D;
test_dict_add([{K,V}|Kvs], D) ->
    show_dict_height(D, splaydict:size(D)),
    test_dict_add(Kvs, splaydict:store(K, V, D)).

test_dict_remove([], D) -> D;
test_dict_remove([{K,_}|Kvs], D) ->
    show_dict_height(D, splaydict:size(D)),
    test_dict_remove(Kvs, splaydict:erase(K, D)).

show_dict_height(D, S) ->
    case S > 0 andalso S rem 10 =:= 0 of
        true -> io:format("~nWith ~p els, height should be lg(~p)=~p. Actual: ~p~n",
                          [ S, S, round(math:log(S)), splaydict:height(D)]);
        _ -> ignore
    end.

test_dict_fetch([], _) -> ok;
test_dict_fetch([{K,V}|Kvs], D) ->
    show_dict_height(D, splaydict:size(D)),
    case splaydict:fetch(K, D) of
        V -> test_dict_fetch(Kvs, D);
        E -> erlang:error({missing_pair, K, V, found, E})
    end.

test_splay() ->
    Els = random_els(10000),
    {_,_} = statistics(runtime),
    Splay0 = test_splay_add(Els, splay:new()),
    {_,AddTime} = statistics(runtime),
    io:format("From ~p els, Splay0 has ~p els~n", [ length(Els), splaysets:size(Splay0)]),
    show_splay_height(Splay0, splay:size(Splay0)),
    {ok, Splay01} = test_splay_random_fetch(Els, Splay0, 1000),
    show_splay_height(Splay01, splay:size(Splay01)),
    {ok, Splay02} = test_splay_random_fetch(Els, Splay01, 1000),
    show_splay_height(Splay02, splay:size(Splay02)),
    {ok, Splay03} = test_splay_random_fetch(Els, Splay02, 1000),
    show_splay_height(Splay03, splay:size(Splay03)),
    {_,_} = statistics(runtime),
    {Splay04, Min} = splay:find_min(Splay03),
    io:format("Min: ~p~n", [Min]),
    show_splay_height(Splay03, splay:size(Splay04)),
    {ok, Splay1} = test_splay_fetch(Els, Splay04),
    show_splay_height(Splay1, splay:size(Splay1)),
    io:format("Test Fetch returned ok ~n", []),
    {_,FetchTime} = statistics(runtime),
    Splay2 = test_splay_remove(Els, Splay1),
    {_,RemoveTime} = statistics(runtime),
    io:format("Removed from splay. Splay now ~p~n", [Splay2]),
    io:format("Add took ~p ms. Fetching took ~p ms. Removal took ~p ms.~n", [AddTime, FetchTime, RemoveTime]),
    ok.

test_splay_add([], Splay) -> show_splay_height(Splay, splay:size(Splay)), Splay;
test_splay_add([E|Es], Splay) ->
    test_splay_add(Es, splay:add(E, Splay)).

test_splay_remove([], Splay) -> Splay;
test_splay_remove([_E|Es], Splay) ->
    {S, _} = splay:delete_min(Splay),
    test_splay_remove(Es, S).

show_splay_height(Splay, N) ->
    io:format("With ~p els, height should be lg(~p)=~p. Actual: ~p~n",
              [ N, N, round(math:log(N)), splay:height(Splay)]).

test_splay_fetch([], S) -> {ok, S};
test_splay_fetch([E|Es], Splay) ->
    case splay:has(E, Splay) of
        {true, S} -> test_splay_fetch(Es, S);
        {false, _S} -> erlang:error({missing_element, E})
    end.

test_splay_random_fetch(_Els, Splay, 0) -> {ok, Splay};
test_splay_random_fetch(Els, Splay, N) ->
    E = lists:nth(random:uniform(length(Els)), Els),
    case splay:has(E, Splay) of
        {true, S} -> test_splay_random_fetch(Els, S, N-1);
        {false, _S} -> erlang:error({missing_element, E})
    end.

random_els(N) -> [ random:uniform() || _ <- lists:seq(1, N) ].
random_kvs(N) -> [ {random:uniform(),I} || I <- lists:seq(1, N) ].
