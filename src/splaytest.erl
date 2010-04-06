-module(splaytest).

-export([test_sets/0]).

test_sets() ->
    Els = random_els(1000000),
    {_,_} = statistics(runtime),
    Set0 = lists:foldl(fun(E, S) -> splaysets:add_element(E, S) end, splaysets:new(), Els),
    {_,AddTime} = statistics(runtime),
    io:format("From ~p els, Set0 has ~p els~n", [ length(Els), splaysets:size(Set0)]),
    io:format("Test Fetch returned: ~p~n", [test_fetch(Els, Set0)]),
    {_,FetchTime} = statistics(runtime),
    Set1 = test_remove(Els, Set0),
    {_,RemoveTime} = statistics(runtime),
    io:format("Removed from set. Set now ~p~n", [Set1]),
    io:format("Add took ~p ms. Fetching took ~p ms. Removal took ~p ms.~n", [AddTime, FetchTime, RemoveTime]),
    ok.

test_remove([], Set) -> Set;
test_remove([E|Es], Set) ->
    test_remove(Es, splaysets:del_element(E, Set)).

test_fetch([], _) -> ok;
test_fetch([E|Es], Set) ->
    case splaysets:is_element(E, Set) of
        true -> test_fetch(Es, Set);
        false -> erlang:error({missing_element, E})
    end.

random_els(N) -> [ random:uniform() || _ <- lists:seq(1, N) ].
random_kvs(N) -> [ {random:uniform(),I} || I <- lists:seq(1, N) ].
