:- use_module(library(term_diff)).
:- use_module(library(apply), [maplist/2]).

% macro to generate goals for all three term_diff/3 modes
term_expansion(Name -> term_diff(A,Diff,B),Tests) :-
    atom_concat(Name, ' (-,+,+)', DashPlusPlus),
    atom_concat(Name, ' (+,-,+)', PlusDashPlus),
    atom_concat(Name, ' (+,+,-)', PlusPlusDash),
    Tests = [
        ( DashPlusPlus :- 
            term_diff(X,Diff,B),
            X = A
        ),
        ( PlusDashPlus :-
            term_diff(A,X,B),
            X = Diff
        ),
        ( PlusPlusDash :-
            term_diff(A,Diff,X),
            X = B
        )
    ],
    maplist(tap:register_test, [DashPlusPlus,PlusDashPlus,PlusPlusDash]).

:- use_module(library(tap)).

% add tests showing common usage
'atomic with two integers' ->
    term_diff(1, atomic(1,2), 2).

'atomic with two floats' ->
    term_diff(1.3, atomic(1.3,97.4), 97.4).

'atomic with different types' ->
    term_diff(1.0, atomic(1.0,1), 1).

'changing atoms' ->
    term_diff(a, name(a,b), b).

'changing functor names' ->
    term_diff(a(first), name(a,b), b(first)).

'dropping a term''s first argument' ->
    term_diff(a(first,second), drop_arg(1,first), a(second)).

'dropping a term''s trailing argument' ->
    term_diff(a(first,second), drop_arg(2,second), a(first)).

'dropping a term''s internal argument' ->
    term_diff(a(first,second,third), drop_arg(2,second), a(first,third)).
