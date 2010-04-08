-module(splay).

-export([new/0, size/1, to_list/1, from_list/1]).
-export([add/2, find_min/1, remove/2, rm_min/1]).

-record(splay, {
          lte={},
          data=undefined,
          gt={}
         }).

%% new() -> Splay()
new() -> empty.

%% size(Splay) -> int()
size(empty) -> 0;
size(#splay{lte=A, gt=B}) -> splay:size(A) + 1 + splay:size(B).

%% to_list(Splay) -> [Element].
to_list(empty) -> [];
to_list(#splay{lte=A, data=X, gt=B}) ->
    lists:flatten([to_list(A), [X], to_list(B)]).

%% from_list([Element]) -> Splay.
from_list(L) -> lists:foldl(fun add/2, new(), L).

%% add(Element, Splay) -> Splay.
add(X, T) ->
    {Lte, Gt} = partition(X, T),
    #splay{lte=Lte, data=X, gt=Gt}.

%% find_min(Splay) -> {Splay, Min}
find_min(#splay{lte=empty, data=Min}=T) ->
    {Lte, Gt} = partition(Min, T),
    {#splay{lte=Lte, data=Min, gt=Gt}, Min};
find_min(#splay{lte=A}) -> find_min(A).

%% remove(Element, Splay) -> Splay
remove(_, empty) -> empty;
remove(X, #splay{lte=A, data=Y}=T) when X < Y ->
    T#splay{lte=remove(X, A)};
remove(X, #splay{data=Y, gt=B}=T) when X > Y ->
    T#splay{gt=remove(X, B)};
remove(X, #splay{lte=A, data=X, gt=B}) ->
    merge(A, B).

%% rm_min(Splay) -> {Splay, Element}.
rm_min(#splay{lte=empty, gt=B}) -> {B, undefined};
rm_min(#splay{lte=#splay{lte=empty, gt=B}, data=Y, gt=C}) ->
    {#splay{lte=B, data=Y, gt=C}, Y};
rm_min(#splay{lte=#splay{lte=A, data=X, gt=B}, data=Y, gt=C}) ->
    {Lte, Min} = rm_min(A),
    {#splay{lte=Lte, data=X, gt=#splay{lte=B, data=Y, gt=C}}, Min}.

merge(empty, T) -> T;
merge(#splay{lte=A, data=X, gt=B}, T) ->
    {TA, TB} = partition(X, T),
    #splay{lte=merge(TA, A), data=X, gt=merge(TB, B)}.

partition(_Pivot, empty) -> {empty,empty};
partition(Pivot, #splay{lte=A, data=X, gt=B}=T) ->
    case X =< Pivot of
        true ->
            case B of
                empty ->
                    {T, B};
                #splay{lte=B1, data=Y, gt=B2} ->
                    case Y =< Pivot of
                        true ->
                            {Lte, Gt} = partition(Pivot, B2),
                            {#splay{lte=#splay{lte=A, data=X, gt=B1}, data=Y, gt=Lte}, Gt};
                        false ->
                            {Lte, Gt} = partition(Pivot, B1),
                            {#splay{lte=A, data=X, gt=Lte}, #splay{lte=Gt, data=Y, gt=B2}}
                    end
            end;
        false ->
            case A of
                empty ->
                    {A, T};
                #splay{lte=A1, data=Y, gt=A2} ->
                    case Y =< Pivot of
                        true ->
                            {Lte, Gt} = partition(Pivot, A2),
                            {#splay{lte=A1, data=Y, gt=Lte}, #splay{lte=Gt, data=X, gt=B}};
                        false ->
                            {Lte, Gt} = partition(Pivot, A1),
                            {Lte, #splay{lte=Gt, data=Y, gt=#splay{lte=A2, data=X, gt=B}}}
                    end
            end
    end.
