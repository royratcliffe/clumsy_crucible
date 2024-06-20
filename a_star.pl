:- use_module(library(heaps), [singleton_heap/3, get_from_heap/4]).

%!  a_star(+Score0, +Node0, :Goal, -Score, -Node) is nondet.
%
%   Non-deterministically expands an A-star heap until empty using the
%   given non-deterministic Goal to find all expanded nodes.

a_star(Score0, Node0, Goal, Score, Node) :-
    a_star_initial(Score0, Node0, Heap),
    a_star_(Heap, Goal, Score, Node, _).

a_star_(Heap0, _Goal, Score, Node, _Heap) :-
    a_star_final(Heap0, Score, Node, _).
a_star_(Heap0, Goal, Score, Node, Heap) :-
    a_star_expand(Heap0, Goal, Heap1),
    heap_size(Heap1, Size1),
    (   Size1 == 0
    ->  true
    ;   a_star_(Heap1, Goal, Score, Node, Heap)
    ).

a_star_initial(Score, Node, Heap) :-
    singleton_heap(Heap, Score, Node).

a_star_final(Heap0, Score, Node, Heap) :-
    get_from_heap(Heap0, Score, Node, Heap).

a_star_expand(Heap0, Goal, Heap) :-
    a_star_expand(Heap0, Goal, Nodes, Heap1),
    heaps:list_to_heap(Nodes, Heap1, Heap).

a_star_expand(Heap0, Goal, Nodes, Heap) :-
    get_from_heap(Heap0, Score0, Node0, Heap),
    findall(Score-Node, call(Goal, Score0, Node0, Score, Node), Nodes).
