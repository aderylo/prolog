ensure_loaded(library(lists)).


trasa(r1, zakopane, brzeziny, rower, oba, 25).
trasa(r2, brzeziny, gasienicowa, rower, oba, 15).
trasa(r3, brzeziny, poroniec, rower, oba, 10).
trasa(r4, poroniec, rusinowa, rower, oba, 6).
trasa(g1, zakopane, kuznice, gorska, oba, 7).
trasa(g2, zakopane, kalatowki, gorska, oba, 5).
trasa(g3, kuznice, gasienicowa, gorska, oba, 7).
trasa(g4, gasienicowa, zawrat, gorska, oba, 6).
trasa(g5, gasienicowa, czarnystaw, gorska, oba, 3).
trasa(g6, zawrat, kozia, gorska, jeden, 5).
trasa(g7, kozia, gasienicowa, gorska, jeden, 7).
trasa(p1, zakopane, gubalowka, piesza, oba, 5).
trasa(d1, a, b, piesza, jeden, 5).
trasa(d2, a, c, piesza, oba, 5).
trasa(d3, b, c, rower, jeden, 5).



% === main  ========================================================================

user:runtime_entry(start):-
  (current_prolog_flag(argv, [File]) ->
    set_prolog_flag(fileerrors, off),
    (
      compile(File) -> true;
	    format('Error opening file ~p.\n', [File])
    ),
	  przetwarzaj;
	  write('Incorrect usage, use: program <file>\n')
  ).

przetwarzaj :-
  processStart(Start),
  processEnd(End), 
  processConditions(Types, LenCondtion),
  (
    (listOfAllPaths(Start, End, Types, LenCondtion, PathL),
      member(_,PathL),
      printPathList(PathL)
    );
    format('Brak trasy z ~p do ~p.~n', [Start, End])
  ),
  przetwarzaj. 

% === input reading =================================================================

processStart(Start) :-
  write('Podaj miejsce startu: '),
  read(Start),
  (
    Start == 'koniec'
    -> write('Koniec programu. Milych wedrowek!\n'), halt
    ; true 
  ). 

processEnd(End) :-
  write('Podaj koniec: '),
  read(End). 

processConditions(Types, LenCondtion) :-
  write('Podaj warunki: '),
  read(Conditions),
  (
    evalConditions(Conditions, Types, LenCondtion)
    -> true
    ; processConditions(Types, LenCondtion)
  ).  

% === input parsing =================================================================

evalConditions(nil, nil, (ge, 0)).

evalConditions(Conditions, Types, LenCondtion) :-
  tupleToList(Conditions, ConditionsList),
  parseConditions(ConditionsList, Types, LenCondtions), 
  parseLenConditions(LenCondtions, LenCondtion),
  write(LenCondtion). 

parseConditions([], [], []).

parseConditions([rodzaj(X) | T], [X | Types], LenCondtions) :-
  parseConditions(T, Types, LenCondtions).

parseConditions([dlugosc(Op, X) | T], Types, [(Op, X) | LenCondtions]) :-
  member(Op, [eq, lt, le, gt, ge]), 
  parseConditions(T, Types, LenCondtions).

parseConditions([E | T], Types, LenCondtions) :-
  format('Error: niepoprawny warunek - ~p\n', [E]),
  false. 


parseLenConditions([], (ge, 0)). 
parseLenConditions([(Op, X) | []], (Op, X)). 
parseLenConditions([(Op, X) | LenCondtions], _) :-
  write('Zbyt wiele warunków na długość'),
  false. 

  

% === path finding =================================================================

findPaths(From, To, Types, LenCondtion, FinalLen, Path):-
  findPaths(From, To, Types, LenCondtion, 0, FinalLen, [], Path).

findPaths(X, X, _Types, (Op, SpecLen), Len, Len, T, T) :- 
  evalFinalLength(Op, SpecLen, Len).

% Path consists of stages: stage(Start, id, type, End)
findPaths(X, Y, Types, LenCondtion, TotalLen, FinalLen, T, NT) :-
  (
    trasa(Id, X, Z, Type, _Dir, Len);
    trasa(Id, Z, X, Type, oba, Len) 
  ),
  (member(Type, Types) ; Types = nil),
  S = stage(X, Id, Type, Z, Len),
  \+ member(S,T),
  findPaths(Z, Y, Types, LenCondtion, TotalLen+Len, FinalLen, [S|T], NT).  

% === find all paths ===============================================================

listOfAllPaths(Start, End, Types, LenCondtion, Loa) :- 
  listOfAllPaths(Start, End, Types, LenCondtion, [], Loa), !.
listOfAllPaths(Start, End, Types, LenCondtion, Acc, Loa) :- 
  dec(Start, End, Types, LenCondtion, Y), 
  uList(Y, Acc, AccNew), 
  listOfAllPaths(Start, End, Types, LenCondtion,AccNew, Loa).
listOfAllPaths(Start, End, Types, LenCondtion, Acc, Acc).

dec(Start, End, Types, LenCondtion, Path) :- 
  findPaths(Start, End, Types, LenCondtion, FinalLen, Path). 

uList(X, [], [X])  :- !.
uList(H, [H|_], _) :- !, fail.
uList(X, [H|T], L) :- uList(X, T, Rtn), L = [H|Rtn].  

% === length criteria ==============================================================

evalFinalLength(eq, SpecLen, ActualLen) :-
  SpecLen =:= ActualLen. 
evalFinalLength(lt, SpecLen, ActualLen) :-
  ActualLen < SpecLen.
evalFinalLength(le, SpecLen, ActualLen) :-
  (ActualLen < SpecLen ; ActualLen =:= SpecLen).
evalFinalLength(gt, SpecLen, ActualLen) :-
  ActualLen > SpecLen.
evalFinalLength(ge, SpecLen, ActualLen) :-
  (ActualLen > SpecLen ; ActualLen =:= SpecLen).

% === aux ====================================================================
% Auxiliary functions used in the program

tupleToList((X, T), [X | L]) :- tupleToList(T,L).
tupleToList(X, [X]) :- X \= (_, _). 

reverse_list([], []).
reverse_list([X|Xs], Reversed) :-
    reverse_list(Xs, ReversedTail),
    append(ReversedTail, [X], Reversed).

pathLen([], 0).
pathLen([stage(Start, Id, Type, End, Len) | []], Len).
pathLen([stage(Start, Id, Type, End, Len) | T], TailLen + Len) :-
  pathLen(T, TailLen).


printPath([]). 
printPath([stage(Start, Id, Type, End, _Len) | []]) :-
  format('~p - (~p, ~p) -> ~p\n', [Start, Id, Type, End]).
printPath([stage(Start, Id, Type, _End, _Len) | T]) :- 
  format('~p - (~p, ~p) ->', [Start, Id, Type]),
  printPath(T). 

printPathList([H | T]) :-
  reverse_list(H, R), 
  pathLen(R, Len), 
  printPath(R),
  format('Dlugosc trasy: ~d\n\n', Len), 
  printPathList(T). 
printPathList(_). 