% Przykładowy program głowny w Prologu

% trasa(r1, zakopane, brzeziny, rower, oba, 25).
% trasa(r2, brzeziny, gasienicowa, rower, oba, 15).
% trasa(r3, brzeziny, poroniec, rower, oba, 10).
% trasa(r4, poroniec, rusinowa, rower, oba, 6).
% trasa(g1, zakopane, kuznice, gorska, oba, 7).
% trasa(g2, zakopane, kalatowki, gorska, oba, 5).
% trasa(g3, kuznice, gasienicowa, gorska, oba, 7).
% trasa(g4, gasienicowa, zawrat, gorska, oba, 6).
% trasa(g5, gasienicowa, czarnystaw, gorska, oba, 3).
% trasa(g6, zawrat, kozia, gorska, jeden, 5).
% trasa(g7, kozia, gasienicowa, gorska, jeden, 7).
% trasa(p1, zakopane, gubalowka, piesza, oba, 5).
% trasa(s1, warszawa, krakow, rower, oba, 50).
% trasa(s2, warszawa, czestochowa, rower, oba, 100).
% trasa(s3, krakow, jura, rower, oba, 30). 
% trasa(s4, jura, czestochowa, piesza, oba, 40). 
ensure_loaded(library(lists)).


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


% właściwe przetwarzanie
 
przetwarzaj :-
  write('Podaj miejsce startu: '),
  read(Start),
  write('Podaj koniec: '),
  read(End),
  write('Podaj warunki: '),
  read(Conditions), 
  evalConditions(Conditions, Types, LenCondtion),
  write('conds all right'), 
  (
    findPaths(Start, End, Types, LenCondtion, Path) ->
    (
      % format('Istnieje trasa dlugosci ~d.~n', [Km]),
      write('Trasa: '), write(Path), nl 
    );
    format('Brak trasy z ~p do ~p.~n', [Start, End])
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


% fix for future me, instead of places keep, transitions to comply with weird wording off
% the task.

findPaths(From, To, Types, LenCondtion, Path):-
  findPaths(From, To, Types, LenCondtion, 0, [], Path).

findPaths(X, X, _Types, (Op, SpecLen), Len, T, T) :- 
  evalFinalLength(Op, SpecLen, Len).

% Path consists of stages: stage(Start, id, type, End)
findPaths(X, Y, Types, LenCondtion, TotalLen, T, NT) :-
  (
    trasa(Id, X, Z, Type, _Dir, Len);
    trasa(Id, Z, X, Type, oba, Len) 
  ),
  (member(Type, Types) ; Types = nil),
  S = stage(X, Id, Type, Z), 
  \+ member(S,T),
  findPaths(Z, Y, Types, LenCondtion, TotalLen+Len, [S|T], NT).  


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