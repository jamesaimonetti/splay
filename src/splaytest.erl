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

random_els(N) -> [ random:uniform() || _ <- lists:seq(1, N) ].
random_kvs(N) -> [ {random:uniform(),I} || I <- lists:seq(1, N) ].
