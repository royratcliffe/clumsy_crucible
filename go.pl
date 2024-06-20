go(X0-Y, >, X-Y) :- succ(X0, X).
go(X-Y0, v, X-Y) :- succ(Y0, Y).
go(X0-Y, <, X-Y) :- succ(X, X0).
go(X-Y0, ^, X-Y) :- succ(Y, Y0).

:- begin_tests(go).
test(it, all(A-B == [(>)-(2-1),v-(1-2),(<)-(0-1),(^)-(1-0)])) :- go(1-1, A, B).
:- end_tests(go).

direction(go(From, To), Direction) :- go(From, Direction, To).

:- begin_tests(direction).
test(it, [A == v, nondet]) :- go(1-1, A, 1-2).
:- end_tests(direction).
