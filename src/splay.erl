-module(splay).

-export([new/0, size/1, height/1, to_list/1, from_list/1]).
-export([add/2, find_min/1, has/2, splay/2]).
-export([merge/2, delete/2, delete_min/1]).

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

%% height(Splay) -> int()
%%   returns the height of the tree
height(empty) -> 0;
height(#splay{lte=A, gt=B}) ->
    Ha = height(A),
    Hb = height(B),
    case Ha > Hb of
        true -> 1 + Ha;
        false -> 1 + Hb
    end.

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

%% has(Element, Splay) -> {Splay, bool()}
has(X, T) ->
    Has = has_aux(X, T),
    {splay(X, T, Has), Has}.

has_aux(_, empty) -> false;
has_aux(X, #splay{lte=A, data=Y}) when X < Y ->
    has_aux(X, A);
has_aux(X, #splay{data=Y, gt=B}) when X > Y ->
    has_aux(X, B);
has_aux(X, #splay{data=X}) -> true.

%% find_min(Splay) -> {Splay, Min}
find_min(T) ->
    Min = find_min_aux(T),
    {splay(Min, T), Min}.

find_min_aux(#splay{lte=empty, data=Min}) -> Min;
find_min_aux(#splay{lte=A}) -> find_min_aux(A).

%% delete(Element, Splay) -> Splay
delete(_, empty) -> empty;
delete(X, #splay{lte=A, data=Y}=T) when X < Y ->
    T#splay{lte=delete(X, A)};
delete(X, #splay{data=Y, gt=B}=T) when X > Y ->
    T#splay{gt=delete(X, B)};
delete(X, #splay{lte=A, data=X, gt=B}) ->
    merge(A, B).

%% delete_min(Splay) -> {Splay, Element}.
delete_min(#splay{lte=empty, gt=B}) -> {B, undefined};
delete_min(#splay{lte=#splay{lte=empty, gt=B}, data=Y, gt=C}) ->
    {#splay{lte=B, data=Y, gt=C}, Y};
delete_min(#splay{lte=#splay{lte=A, data=X, gt=B}, data=Y, gt=C}) ->
    {Lte, Min} = delete_min(A),
    {#splay{lte=Lte, data=X, gt=#splay{lte=B, data=Y, gt=C}}, Min}.

merge(empty, T) -> T;
merge(#splay{lte=A, data=X, gt=B}, T) ->
    {TA, TB} = partition(X, T),
    #splay{lte=merge(TA, A), data=X, gt=merge(TB, B)}.

%% splay(Element, Splay) -> Splay
%%   If element exists, Moves node with Element to root of splay; otherwise just re-arranges the tree
splay(X, T) ->
    splay(X, T, has_aux(X, T)).

%% short-circuit splay/2 when has_aux() is known
splay(X, T, true) ->
    {Lte, Gt} = partition(X, T),
    #splay{lte=delete(X, Lte), data=X, gt=Gt};
splay(X, T, false) ->
    {Lte, Gt} = partition(X, T),
    merge(Lte, Gt).

%% partition isn't concerned if there's duplicate data in the tree
partition(_Pivot, empty) -> {empty,empty};
partition(Pivot, #splay{lte=A, data=X, gt=B}=T) when X =< Pivot ->
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
partition(Pivot, #splay{lte=A, data=X, gt=B}=T) ->
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
    end.
