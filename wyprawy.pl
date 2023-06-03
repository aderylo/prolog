ensure_loaded(library(lists)).


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
    findPaths(Start, End, Types, LenCondtion, FinalLen, Path) ->
    (
      reverse_list(Path, RPath), 
      printPath(RPath),
      format('Dlugosc trasy: ~d.~n', [FinalLen])     
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
    -> write('Koniec programu. Milych wedrowek!\n')
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

evalConditions(Conditions, Types, LenCondtions) :-
  tupleToList(Conditions, ConditionsList),
  parseConditions(ConditionsList, Types, LenCondtions).

parseConditions([], [], []).

parseConditions([rodzaj(X) | T], [rodzaj(X)| Types], LenCondtions) :-
  parseConditions(T, Types, LenCondtions).

parseConditions([dlugosc(Op, X) | T], Types, [dlugosc(Op, X) | LenCondtions]) :-
  member(Op, [eq, lt, le, gt, ge]), 
  parseConditions(T, Types, LenCondtions).

parseConditions([E | T], Types, LenCondtions) :-
  format('Error: niepoprawny warunek - ~p\n', [E]),
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
  S = stage(X, Id, Type, Z),
  \+ member(S,T),
  findPaths(Z, Y, Types, LenCondtion, TotalLen+Len, FinalLen, [S|T], NT).  


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

printPath([]). 
printPath([stage(Start, Id, Type, End) | []]) :-
  format('~p - (~p, ~p) -> ~p\n', [Start, Id, Type, End]).
printPath([stage(Start, Id, Type, _End) | T]) :- 
  format('~p - (~p, ~p) ->', [Start, Id, Type]),
  printPath(T). 