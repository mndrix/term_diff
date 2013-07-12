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
'alter with two integers' ->
    term_diff(1, alter(1,2), 2).

'alter with two floats' ->
    term_diff(1.3, alter(1.3,97.4), 97.4).

'alter with different types' ->
    term_diff(1.0, alter(1.0,1), 1).

'alter with nothing shared' ->
    term_diff(a(1), alter(a(1), b(2)), b(2)).

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

'add an initial argument' ->
    term_diff(a(second), add_arg(1,first), a(first,second)).
'add a trailing argument' ->
    term_diff(a(first), add_arg(2,second), a(first,second)).
'add an internal argument' ->
    term_diff(a(first,third), add_arg(2,second), a(first,second,third)).

'add argument to an atom' ->
    term_diff(hello, add_arg(1,world), hello(world)).

'identical integers' ->
    term_diff(1, [], 1).
'identical floats' ->
    term_diff(4.2, [], 4.2).
'identical atoms' ->
    term_diff(howdy, [], howdy).
'identical compound terms' ->
    term_diff(once(upon,a,time), [], once(upon,a,time)).

'sequential patches' ->
    term_diff( n(one,zwei)
             , [ drop_arg(2,zwei)
               , add_arg(2,two)
               , add_arg(3,three)
               ]
             , n(one,two,three)
             ).
