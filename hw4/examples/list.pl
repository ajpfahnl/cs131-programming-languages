unique_list(List, N) :- 
    length(List, N),
    elements_between(List, 1, N),
    all_unique(List).

elements_between(List, Min, Max) :- 
    maplist(between(Min, Max), List).

all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).

unique_list2(List, N) :-
    length(List, N),
    fd_domain(List, 1, N),
    fd_all_different(List),
    fd_labeling(List).