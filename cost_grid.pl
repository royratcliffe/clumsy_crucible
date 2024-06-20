:- use_module(library(dcg/high_order), [sequence//2, sequence//5]).
:- use_module(library(dcg/basics), [blank//0, blanks//0]).

cost(Cost) --> [Code], { code_type(Code, digit(Cost)), ! }.
cost(-) --> "-".

cost_grid(CostGrid) -->
    sequence([], sequence(cost), (blank, blanks), [], CostGrid).

:- begin_tests(cost_grid).
test(it, CostGrid == [[]]) :- phrase(cost_grid(CostGrid), ``).
test(it, CostGrid == [[1, 2, 3], []]) :- phrase(cost_grid(CostGrid), `123\n`).
test(it, CostGrid == [[1, -, 3]]) :- phrase(cost_grid(CostGrid), `1-3`).
:- end_tests(cost_grid).
