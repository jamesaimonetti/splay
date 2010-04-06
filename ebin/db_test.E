-file("./db_test.erl", 1).

scan_vals(0, N) ->
    [ 
     X ||
         {_,X} <-
             lists:sort([ 
                         {random:uniform(N),X} ||
                             X <- lists:seq(1, N)
                        ])
    ];
scan_vals(1, N) ->
    lists:seq(1, N);
scan_vals(2, N) ->
    M = N div 2,
    interlace(lists:seq(M, 1, - 1), lists:seq(M + 1, N, 1));
scan_vals(3, N) ->
    M = N div 2,
    interlace(lists:seq(1, M, 1), lists:seq(M + 1, N, 1));
scan_vals(4, N) ->
    lists:seq(1, N, 2) ++ lists:seq(2, N, 2);
scan_vals(5, N) ->
    lists:seq(N, 1, - 1).

interlace([H1|T1], [H2|T2]) ->
    [H1,H2|interlace(T1, T2)];
interlace([], L2) ->
    L2;
interlace(L1, []) ->
    L1.

null(Reps, Ks, D) when Reps > 1 ->
    null1(Ks, D),
    null(Reps - 1, Ks, D);
null(1, Ks, D) ->
    null1(Ks, D).

null1([K|Ks], D) ->
    nullop({foo,bar,K}, D),
    null1(Ks, D);
null1([], D) ->
    D.

nullop(_, _) ->
    ok.

dict(Reps, Ks) ->
    D0 = dict:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = d_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = d_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = d_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = d_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

d_sto(Reps, Ks, D) when Reps > 1 ->
    d_sto1(Ks, D),
    d_sto(Reps - 1, Ks, D);
d_sto(1, Ks, D) ->
    d_sto1(Ks, D).

d_sto1([K|Ks], D0) ->
    D1 = dict:store({foo,bar,K}, stuff, D0),
    d_sto1(Ks, D1);
d_sto1([], D) ->
    D.

d_fet(Reps, Ks, D) when Reps > 1 ->
    d_fet1(Ks, D),
    d_fet(Reps - 1, Ks, D);
d_fet(1, Ks, D) ->
    d_fet1(Ks, D).

d_fet1([K|Ks], D) ->
    dict:fetch({foo,bar,K}, D),
    d_fet1(Ks, D);
d_fet1([], D) ->
    D.

d_era(Reps, Ks, D) when Reps > 1 ->
    d_era1(Ks, D),
    d_era(Reps - 1, Ks, D);
d_era(1, Ks, D) ->
    d_era1(Ks, D).

d_era1([K|Ks], D0) ->
    D1 = dict:erase({foo,bar,K}, D0),
    d_era1(Ks, D1);
d_era1([], D) ->
    D.

dict1(Reps, Ks) ->
    D0 = dict1:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = d1_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = d1_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = d1_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = d1_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

d1_sto(Reps, Ks, D) when Reps > 1 ->
    d1_sto1(Ks, D),
    d1_sto(Reps - 1, Ks, D);
d1_sto(1, Ks, D) ->
    d1_sto1(Ks, D).

d1_sto1([K|Ks], D0) ->
    D1 = dict1:store({foo,bar,K}, stuff, D0),
    d1_sto1(Ks, D1);
d1_sto1([], D) ->
    D.

d1_fet(Reps, Ks, D) when Reps > 1 ->
    d1_fet1(Ks, D),
    d1_fet(Reps - 1, Ks, D);
d1_fet(1, Ks, D) ->
    d1_fet1(Ks, D).

d1_fet1([K|Ks], D) ->
    dict1:fetch({foo,bar,K}, D),
    d1_fet1(Ks, D);
d1_fet1([], D) ->
    D.

d1_era(Reps, Ks, D) when Reps > 1 ->
    d1_era1(Ks, D),
    d1_era(Reps - 1, Ks, D);
d1_era(1, Ks, D) ->
    d1_era1(Ks, D).

d1_era1([K|Ks], D0) ->
    D1 = dict1:erase({foo,bar,K}, D0),
    d1_era1(Ks, D1);
d1_era1([], D) ->
    D.

dict2(Reps, Ks) ->
    D0 = dict2:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = d2_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = d2_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = d2_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = d2_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

d2_sto(Reps, Ks, D) when Reps > 1 ->
    d2_sto1(Ks, D),
    d2_sto(Reps - 1, Ks, D);
d2_sto(1, Ks, D) ->
    d2_sto1(Ks, D).

d2_sto1([K|Ks], D0) ->
    D1 = dict2:store({foo,bar,K}, stuff, D0),
    d2_sto1(Ks, D1);
d2_sto1([], D) ->
    D.

d2_fet(Reps, Ks, D) when Reps > 1 ->
    d2_fet1(Ks, D),
    d2_fet(Reps - 1, Ks, D);
d2_fet(1, Ks, D) ->
    d2_fet1(Ks, D).

d2_fet1([K|Ks], D) ->
    dict2:fetch({foo,bar,K}, D),
    d2_fet1(Ks, D);
d2_fet1([], D) ->
    D.

d2_era(Reps, Ks, D) when Reps > 1 ->
    d2_era1(Ks, D),
    d2_era(Reps - 1, Ks, D);
d2_era(1, Ks, D) ->
    d2_era1(Ks, D).

d2_era1([K|Ks], D0) ->
    D1 = dict2:erase({foo,bar,K}, D0),
    d2_era1(Ks, D1);
d2_era1([], D) ->
    D.

orddict(Reps, Ks) ->
    D0 = orddict:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = o_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = o_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = o_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = o_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

o_sto(Reps, Ks, D) when Reps > 1 ->
    o_sto1(Ks, D),
    o_sto(Reps - 1, Ks, D);
o_sto(1, Ks, D) ->
    o_sto1(Ks, D).

o_sto1([K|Ks], D0) ->
    D1 = orddict:store({foo,bar,K}, stuff, D0),
    o_sto1(Ks, D1);
o_sto1([], D) ->
    D.

o_fet(Reps, Ks, D) when Reps > 1 ->
    o_fet1(Ks, D),
    o_fet(Reps - 1, Ks, D);
o_fet(1, Ks, D) ->
    o_fet1(Ks, D).

o_fet1([K|Ks], D) ->
    orddict:fetch({foo,bar,K}, D),
    o_fet1(Ks, D);
o_fet1([], D) ->
    D.

o_era(Reps, Ks, D) when Reps > 1 ->
    o_era1(Ks, D),
    o_era(Reps - 1, Ks, D);
o_era(1, Ks, D) ->
    o_era1(Ks, D).

o_era1([K|Ks], D0) ->
    D1 = orddict:erase({foo,bar,K}, D0),
    o_era1(Ks, D1);
o_era1([], D) ->
    D.

rbdict(Reps, Ks) ->
    D0 = rbdict:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = r_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = r_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = r_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = r_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

r_sto(Reps, Ks, D) when Reps > 1 ->
    r_sto1(Ks, D),
    r_sto(Reps - 1, Ks, D);
r_sto(1, Ks, D) ->
    r_sto1(Ks, D).

r_sto1([K|Ks], D0) ->
    D1 = rbdict:store({foo,bar,K}, stuff, D0),
    r_sto1(Ks, D1);
r_sto1([], D) ->
    D.

r_fet(Reps, Ks, D) when Reps > 1 ->
    r_fet1(Ks, D),
    r_fet(Reps - 1, Ks, D);
r_fet(1, Ks, D) ->
    r_fet1(Ks, D).

r_fet1([K|Ks], D) ->
    rbdict:fetch({foo,bar,K}, D),
    r_fet1(Ks, D);
r_fet1([], D) ->
    D.

r_era(Reps, Ks, D) when Reps > 1 ->
    r_era1(Ks, D),
    r_era(Reps - 1, Ks, D);
r_era(1, Ks, D) ->
    r_era1(Ks, D).

r_era1([K|Ks], D0) ->
    D1 = rbdict:erase({foo,bar,K}, D0),
    r_era1(Ks, D1);
r_era1([], D) ->
    D.

rbdict1(Reps, Ks) ->
    D0 = rbdict1:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = r1_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = r1_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = r1_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = r1_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

r1_sto(Reps, Ks, D) when Reps > 1 ->
    r1_sto1(Ks, D),
    r1_sto(Reps - 1, Ks, D);
r1_sto(1, Ks, D) ->
    r1_sto1(Ks, D).

r1_sto1([K|Ks], D0) ->
    D1 = rbdict1:store({foo,bar,K}, stuff, D0),
    r1_sto1(Ks, D1);
r1_sto1([], D) ->
    D.

r1_fet(Reps, Ks, D) when Reps > 1 ->
    r1_fet1(Ks, D),
    r1_fet(Reps - 1, Ks, D);
r1_fet(1, Ks, D) ->
    r1_fet1(Ks, D).

r1_fet1([K|Ks], D) ->
    rbdict1:fetch({foo,bar,K}, D),
    r1_fet1(Ks, D);
r1_fet1([], D) ->
    D.

r1_era(Reps, Ks, D) when Reps > 1 ->
    r1_era1(Ks, D),
    r1_era(Reps - 1, Ks, D);
r1_era(1, Ks, D) ->
    r1_era1(Ks, D).

r1_era1([K|Ks], D0) ->
    D1 = rbdict1:erase({foo,bar,K}, D0),
    r1_era1(Ks, D1);
r1_era1([], D) ->
    D.

rbdict2(Reps, Ks) ->
    D0 = rbdict2:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = r2_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = r2_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = r2_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = r2_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

r2_sto(Reps, Ks, D) when Reps > 1 ->
    r2_sto2(Ks, D),
    r2_sto(Reps - 1, Ks, D);
r2_sto(1, Ks, D) ->
    r2_sto2(Ks, D).

r2_sto2([K|Ks], D0) ->
    D1 = rbdict2:store({foo,bar,K}, stuff, D0),
    r2_sto2(Ks, D1);
r2_sto2([], D) ->
    D.

r2_fet(Reps, Ks, D) when Reps > 1 ->
    r2_fet2(Ks, D),
    r2_fet(Reps - 1, Ks, D);
r2_fet(1, Ks, D) ->
    r2_fet2(Ks, D).

r2_fet2([K|Ks], D) ->
    rbdict2:fetch({foo,bar,K}, D),
    r2_fet2(Ks, D);
r2_fet2([], D) ->
    D.

r2_era(Reps, Ks, D) when Reps > 1 ->
    r2_era2(Ks, D),
    r2_era(Reps - 1, Ks, D);
r2_era(1, Ks, D) ->
    r2_era2(Ks, D).

r2_era2([K|Ks], D0) ->
    D1 = rbdict2:erase({foo,bar,K}, D0),
    r2_era2(Ks, D1);
r2_era2([], D) ->
    D.

rbdict3(Reps, Ks) ->
    D0 = rbdict3:new(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = r3_sto(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = r3_fet(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D3 = r3_sto(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D4 = r3_era(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    {Tn / Nreps,Ti / Reps,Tf / Reps,Tu / Reps,Te / Reps}.

r3_sto(Reps, Ks, D) when Reps > 1 ->
    r3_sto3(Ks, D),
    r3_sto(Reps - 1, Ks, D);
r3_sto(1, Ks, D) ->
    r3_sto3(Ks, D).

r3_sto3([K|Ks], D0) ->
    D1 = rbdict3:store({foo,bar,K}, stuff, D0),
    r3_sto3(Ks, D1);
r3_sto3([], D) ->
    D.

r3_fet(Reps, Ks, D) when Reps > 1 ->
    r3_fet3(Ks, D),
    r3_fet(Reps - 1, Ks, D);
r3_fet(1, Ks, D) ->
    r3_fet3(Ks, D).

r3_fet3([K|Ks], D) ->
    rbdict3:fetch({foo,bar,K}, D),
    r3_fet3(Ks, D);
r3_fet3([], D) ->
    D.

r3_era(Reps, Ks, D) when Reps > 1 ->
    r3_era3(Ks, D),
    r3_era(Reps - 1, Ks, D);
r3_era(1, Ks, D) ->
    r3_era3(Ks, D).

r3_era3([K|Ks], D0) ->
    D1 = rbdict3:erase({foo,bar,K}, D0),
    r3_era3(Ks, D1);
r3_era3([], D) ->
    D.

gb_trees(Reps, Ks) ->
    D0 = gb_trees:empty(),
    Nreps = 10000000 div length(Ks) + 1,
    {_,_} = statistics(runtime),
    null(Nreps, Ks, D0),
    {_,Tn} = statistics(runtime),
    D1 = g_ins(Reps, Ks, D0),
    {_,Ti} = statistics(runtime),
    _D2 = g_ent(Reps, Ks, D0),
    {_,Ti1} = statistics(runtime),
    _D3 = g_get(Reps, Ks, D1),
    {_,Tf} = statistics(runtime),
    _D4 = g_upd(Reps, Ks, D1),
    {_,Tu} = statistics(runtime),
    _D5 = g_ent(Reps, Ks, D1),
    {_,Tu1} = statistics(runtime),
    _D6 = g_del(Reps, Ks, D1),
    {_,Te} = statistics(runtime),
    _D7 = g_del_a(Reps, Ks, D1),
    {_,Te1} = statistics(runtime),
    {Tn / Nreps,
     {Ti / Reps,Ti1 / Reps},
     Tf / Reps,
     {Tu / Reps,Tu1 / Reps},
     {Te / Reps,Te1 / Reps}}.

g_ins(Reps, Ks, D) when Reps > 1 ->
    g_ins1(Ks, D),
    g_ins(Reps - 1, Ks, D);
g_ins(1, Ks, D) ->
    g_ins1(Ks, D).

g_ins1([K|Ks], D0) ->
    D1 = gb_trees:insert({foo,bar,K}, stuff, D0),
    g_ins1(Ks, D1);
g_ins1([], D) ->
    D.

g_ent(Reps, Ks, D) when Reps > 1 ->
    g_ent1(Ks, D),
    g_ent(Reps - 1, Ks, D);
g_ent(1, Ks, D) ->
    g_ent1(Ks, D).

g_ent1([K|Ks], D0) ->
    D1 = gb_trees:enter({foo,bar,K}, stuff, D0),
    g_ent1(Ks, D1);
g_ent1([], D) ->
    D.

g_upd(Reps, Ks, D) when Reps > 1 ->
    g_upd1(Ks, D),
    g_upd(Reps - 1, Ks, D);
g_upd(1, Ks, D) ->
    g_upd1(Ks, D).

g_upd1([K|Ks], D0) ->
    D1 = gb_trees:update({foo,bar,K}, stuff, D0),
    g_upd1(Ks, D1);
g_upd1([], D) ->
    D.

g_get(Reps, Ks, D) when Reps > 1 ->
    g_get1(Ks, D),
    g_get(Reps - 1, Ks, D);
g_get(1, Ks, D) ->
    g_get1(Ks, D).

g_get1([K|Ks], D) ->
    gb_trees:get({foo,bar,K}, D),
    g_get1(Ks, D);
g_get1([], D) ->
    D.

g_del(Reps, Ks, D) when Reps > 1 ->
    g_del1(Ks, D),
    g_del(Reps - 1, Ks, D);
g_del(1, Ks, D) ->
    g_del1(Ks, D).

g_del1([K|Ks], D0) ->
    D1 = gb_trees:delete({foo,bar,K}, D0),
    g_del1(Ks, D1);
g_del1([], D) ->
    D.

g_del_a(Reps, Ks, D) when Reps > 1 ->
    g_del_a1(Ks, D),
    g_del_a(Reps - 1, Ks, D);
g_del_a(1, Ks, D) ->
    g_del_a1(Ks, D).

g_del_a1([K|Ks], D0) ->
    D1 = gb_trees:delete_any({foo,bar,K}, D0),
    g_del_a1(Ks, D1);
g_del_a1([], D) ->
    D.

module_info() ->
    erlang:get_module_info(db_test).

module_info(X) ->
    erlang:get_module_info(db_test, X).

