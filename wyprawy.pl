% Przykładowy program głowny w Prologu

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

user:runtime_entry(start):-
    (current_prolog_flag(argv, [File]) ->
        set_prolog_flag(fileerrors, off),
        (compile(File) -> true
         ;
	 format('Error opening file ~p.\n', [File])
        ),
        prompt(_Old, ''),         % pusty prompt
	przetwarzaj
     ;
	write('Incorrect usage, use: program <file>\n')
    ).


% właściwe przetwarzanie

przetwarzaj :-
    write('Podaj miejsce startu: '),
    read(Start),
    write('Podaj koniec: '),
    read(Meta),
    (
      trasa(_Id, Start, Meta, _Rodzaj, _Kierunek, Km) ->
      format('Istnieje trasa dlugosci ~d.~n', [Km])
    ;
      format('Brak trasy z ~p do ~p.~n', [Start, Meta])
    ).

findPaths(From, To, Path):-
  findPaths(From, To, [], Path).

findPaths(X, X, T, T).
findPaths(X, Y, T, NT) :-
    trasa(_Id, X, Z, _Type, _Dir, _Len), 
    \+ member(Z,T),
    findPaths(Z, Y, [Z|T], NT).   
