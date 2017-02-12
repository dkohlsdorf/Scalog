greater(1, 0).
greater(2, 0).
greater(2, 1).
greater(3, 0).
greater(3, 1).
greater(3, 2).
greater(4, 0).
greater(4, 1).
greater(4, 2).
greater(4, 3).

smaller(0, 0).
smaller(0, 1).
smaller(0, 2).
smaller(0, 3).
smaller(0, 4).
smaller(1, 1).
smaller(1, 2).
smaller(1, 3).
smaller(1, 4).
smaller(2, 2).
smaller(2, 3).
smaller(2, 4).
smaller(3, 3).
smaller(3, 4).
smaller(4, 4).

quicksort([X|Xs],Ys):-
    partition(Xs,X,Left,Right),
    quicksort(Left,Ls),
    quicksort(Right,Rs),
    append(Ls,[X|Rs],Ys).
quicksort([],[]).

partition([X|Xs],Y,[X|Ls],Rs):-
    smaller(X,Y),
    partition(Xs,Y,Ls,Rs).
partition([X|Xs],Y,Ls,[X|Rs]):-
    greater(X,Y),
    partition(Xs,Y,Ls,Rs).
partition([],Y,[],[]).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-
    append(Xs,Ys,Zs).

