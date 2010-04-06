-module(db_test).

-export([scan_vals/2]).
-compile([export_all]).

-import(lists, [reverse/2,seq/2,seq/3,sort/1]).

%% Define different types/sizes of keys.
%%-define(KEY(N), N).
-define(KEY(N), {foo,bar,N}).

%% Define the top level function with standard test cases.
-define(DTEST(TF, NF, SF, FF, EF),
	TF(Reps, Ks) ->
	       %% Initial setup
	       D0 = NF(),
	       %% Null rep count, must be larger to give good results.
	       Nreps = (10000000 div length(Ks)) + 1,	%Long timing
	       {_,_} = statistics(runtime),
	       %% Null sweep
	       null(Nreps, Ks, D0),
	       {_,Tn} = statistics(runtime),
	       %% Initial store
	       D1 = SF(Reps, Ks, D0),
	       {_,Ti} = statistics(runtime),
	       %% Fetching values
	       _D2 = FF(Reps, Ks, D1),
	       {_,Tf} = statistics(runtime),
	       %% Updating table
	       _D3 = SF(Reps, Ks, D1),
	       {_,Tu} = statistics(runtime),
	       %% Erasing table.
	       _D4 = EF(Reps, Ks, D1),
	       {_,Te} = statistics(runtime),
	       {Tn/Nreps,Ti/Reps,Tf/Reps,Tu/Reps,Te/Reps}).

%% Define a loop to call function a repeat count number of times.
-define(RLOOP(F1, F2),
	F1(Reps, Ks, D) when Reps > 1 ->
	       F2(Ks, D),
	       F1(Reps-1, Ks, D);
	F1(1, Ks, D) -> F2(Ks, D)).

%% Define a loop over list of pairs to call a "store" function which
%% reuses returned updated db.
-define(STORE(F1, F2),
	F1([K|Ks], D0) ->
	       D1 = F2(?KEY(K), stuff, D0),
	       F1(Ks, D1);
	F1([], D) -> D).

%% Define a loop over list of pairs to call a "fetch" function which
%% ignores returned value.
-define(FETCH(F1, F2),
	F1([K|Ks], D) ->
	       F2(?KEY(K), D),
	       F1(Ks, D);
	F1([], D) -> D).

%% Define a loop over list of pairs to call an "erase" function which
%% reuses returned updated db.
-define(ERASE(F1, F2),
	F1([K|Ks], D0) ->
	       D1 = F2(?KEY(K), D0),
	       F1(Ks, D1);
	F1([], D) -> D).

%% scan_vals(Type, Count) -> [{Key,Val}].
%% Generate a list of key-value pairs with the given order

scan_vals(0, N) ->				%Random
    [ X || {_,X} <- sort([ {random:uniform(N),X} || X <- seq(1, N) ]) ];
scan_vals(1, N) -> seq(1, N);			%Straight upwards sweep
scan_vals(2, N) ->				%Middle out
    M = N div 2,
    interlace(seq(M, 1, -1), seq(M+1, N, 1));		  
scan_vals(3, N) ->				%Half and half
    M = N div 2,
    interlace(seq(1, M, 1), seq(M+1, N, 1));
scan_vals(4, N) ->				%Double sweep
    seq(1, N, 2) ++ seq(2, N, 2);
scan_vals(5, N) -> seq(N, 1, -1).		%Straight downwards sweep

interlace([H1|T1], [H2|T2]) -> [H1,H2|interlace(T1, T2)];
interlace([], L2) -> L2;
interlace(L1, []) -> L1.

%% The null loop for base case.
?RLOOP(null, null1).
?FETCH(null1, nullop).

nullop(_, _) -> ok.

?DTEST(dict, dict:new, d_sto, d_fet, d_era).

?RLOOP(d_sto, d_sto1).
?STORE(d_sto1, dict:store).

?RLOOP(d_fet, d_fet1).
?FETCH(d_fet1, dict:fetch).

?RLOOP(d_era, d_era1).
?ERASE(d_era1, dict:erase).

?DTEST(dict1, dict1:new, d1_sto, d1_fet, d1_era).

?RLOOP(d1_sto, d1_sto1).
?STORE(d1_sto1, dict1:store).

?RLOOP(d1_fet, d1_fet1).
?FETCH(d1_fet1, dict1:fetch).

?RLOOP(d1_era, d1_era1).
?ERASE(d1_era1, dict1:erase).

?DTEST(dict2, dict2:new, d2_sto, d2_fet, d2_era).

?RLOOP(d2_sto, d2_sto1).
?STORE(d2_sto1, dict2:store).

?RLOOP(d2_fet, d2_fet1).
?FETCH(d2_fet1, dict2:fetch).

?RLOOP(d2_era, d2_era1).
?ERASE(d2_era1, dict2:erase).

?DTEST(orddict, orddict:new, o_sto, o_fet, o_era).

?RLOOP(o_sto, o_sto1).
?STORE(o_sto1, orddict:store).

?RLOOP(o_fet, o_fet1).
?FETCH(o_fet1, orddict:fetch).

?RLOOP(o_era, o_era1).
?ERASE(o_era1, orddict:erase).

?DTEST(rbdict, rbdict:new, r_sto, r_fet, r_era).

?RLOOP(r_sto, r_sto1).
?STORE(r_sto1, rbdict:store).

?RLOOP(r_fet, r_fet1).
?FETCH(r_fet1, rbdict:fetch).

?RLOOP(r_era, r_era1).
?ERASE(r_era1, rbdict:erase).

?DTEST(rbdict1, rbdict1:new, r1_sto, r1_fet, r1_era).

?RLOOP(r1_sto, r1_sto1).
?STORE(r1_sto1, rbdict1:store).

?RLOOP(r1_fet, r1_fet1).
?FETCH(r1_fet1, rbdict1:fetch).

?RLOOP(r1_era, r1_era1).
?ERASE(r1_era1, rbdict1:erase).

?DTEST(rbdict2, rbdict2:new, r2_sto, r2_fet, r2_era).

?RLOOP(r2_sto, r2_sto2).
?STORE(r2_sto2, rbdict2:store).

?RLOOP(r2_fet, r2_fet2).
?FETCH(r2_fet2, rbdict2:fetch).

?RLOOP(r2_era, r2_era2).
?ERASE(r2_era2, rbdict2:erase).

?DTEST(rbdict3, rbdict3:new, r3_sto, r3_fet, r3_era).

?RLOOP(r3_sto, r3_sto3).
?STORE(r3_sto3, rbdict3:store).

?RLOOP(r3_fet, r3_fet3).
?FETCH(r3_fet3, rbdict3:fetch).

?RLOOP(r3_era, r3_era3).
?ERASE(r3_era3, rbdict3:erase).

%% Special case for gb_trees to test cases for both when it is known
%% and not known that corresponding value already exist. Gb_trees
%% gives different results for this.

gb_trees(Reps, Ks) ->
    D0 = gb_trees:empty(),
    Nreps = (10000000 div length(Ks)) + 1,	%Long timing
    {_,_} = statistics(runtime),
    %% Null sweep
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    %% Initial store
    D1 = g_ins(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = g_ent(Reps, Ks, D0),
    {_,Ti1} = statistics(runtime),
    %% Fetching values
    _D3 = g_get(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    %% Updating table
    _D4 = g_upd(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D5 = g_ent(Reps, Ks, D1),
    {_,Tu1} = statistics(runtime),
    %% Erasing table.
    _D6 = g_del(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    _D7 = g_del_a(Reps, Ks, D1),
    {_,Te1} = statistics(runtime),
    {Tn/Nreps,{Ti/Reps,Ti1/Reps},Tf/Reps,
     {Tu/Reps,Tu1/Reps},{Te/Reps,Te1/Reps}}.

?RLOOP(g_ins, g_ins1).
?STORE(g_ins1, gb_trees:insert).

?RLOOP(g_ent, g_ent1).
?STORE(g_ent1, gb_trees:enter).

?RLOOP(g_upd, g_upd1).
?STORE(g_upd1, gb_trees:update).

?RLOOP(g_get, g_get1).
?FETCH(g_get1, gb_trees:get).

?RLOOP(g_del, g_del1).
?ERASE(g_del1, gb_trees:delete).

?RLOOP(g_del_a, g_del_a1).
?ERASE(g_del_a1, gb_trees:delete_any).
