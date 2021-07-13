cs131sort(L,P) :- permute(L,P), sorted(P).
    permute([], []).
    permute([X|L], PX) :-
        permute(L,P),
        cs131append(P1,P2,P),
        cs131append(P1,[X|P2],PX).
    cs131append([], L, L).
    cs131append([X|L], M, [X|LM]) :- cs131append(L,M,LM).
sorted([]).
sorted([_]).
sorted([X,Y|L]) :- X=<Y, sorted([Y|L]).
