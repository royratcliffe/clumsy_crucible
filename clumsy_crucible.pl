:- ensure_loaded(cost_grid).
:- ensure_loaded(cost_at).
:- ensure_loaded(go).
:- ensure_loaded(a_star).
:- ensure_loaded(elven).
:- set_prolog_flag(stack_limit, 34_359_738_368).
:- initialization(run_tests, after_load).
:- initialization(time(main), after_load).

main :-
    cost_grid_from_clumsy_crucible_txt(CostGrid),
    elven_star(1-1, 13-13, CostGrid, Score, Where),
    write_cost_grid_where(CostGrid, Where),
    write(Score),
    nl.

write_cost_grid_where(CostGrid, Where) :-
    forall(between(1, 13, Y),
           (   forall(between(1, 13, X),
                      (   (   memberchk(go(_, Direction, X-Y), Where)
                          ->  write(Direction)
                          ;   cost_at(CostGrid, X-Y, Cost)
                          ->  write(Cost)
                          ;   write(-)
                          )
                      )),
               nl
           )).
