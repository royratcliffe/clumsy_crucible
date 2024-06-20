cost_grid_from_file(CostGrid, File) :-
    phrase_from_file(cost_grid(CostGrid), File).

cost_grid_from_clumsy_crucible_txt(CostGrid) :-
    cost_grid_from_file(CostGrid, 'clumsy_crucible.txt').

cost_at(CostGrid, X-Y, Cost) :-
    nth1(Y, CostGrid, Costs),
    nth1(X, Costs, Cost),
    Cost \== (-).

:- begin_tests(cost_at).
test(it, Costs == [2, 3]) :-
    cost_grid_from_clumsy_crucible_txt(CostGrid),
    convlist(cost_at(CostGrid), [0-0, 1-1, 13-13], Costs).
:- end_tests(cost_at).
