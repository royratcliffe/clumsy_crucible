elven_star(Initial, Final, CostGrid, Score, Where) :-
    a_star(0, elven(Initial, CostGrid, []),
           elven_expansion,
           Score, elven(Final, _, Where0)),
    reverse(Where0, Where).

elven_expansion(Score0, elven(From, CostGrid, Where),
                Score, elven(To, CostGrid, [go(From, Direction, To)|Where])) :-
    go(From, Direction, To),
    \+ memberchk(go(_, _, To), Where),
    elven_direction(<, Direction, Where),
    elven_direction(^, Direction, Where),
    \+ elven_thrice_in_same_direction(Where, Direction),
    cost_at(CostGrid, To, Cost),
    Score is Score0 + Cost.

elven_direction(Direction, Direction, Where) :-
    !,
    (   memberchk(go(_, Direction, _), Where)
    ->  fail
    ;   true
    ).
elven_direction(_, _, _).

elven_thrice_in_same_direction(Where, Direction) :-
    maplist(arg(2), Where, [Direction, Direction, Direction|_]).
