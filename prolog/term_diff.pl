:- module(term_diff, [term_diff/3]).
:- use_module(library(lcs), [lcs/5]).
:- use_module(library(when), [when/2]).

term_diff(A, atomic(A,B), B) :-
    dif(TypeA, TypeB),
    term_type(A, TypeA),
    term_type(B, TypeB),
    !.
term_diff(A, atomic(A,B), B) :-
    term_type(A, Type),
    term_type(B, Type),
    memberchk(Type,[integer,float]),
    !.
term_diff(A, name(NameA,NameB), B) :-
    term_type(A, callable),
    term_type(B, callable),
    lazy_univ(A, NameA, Arguments),
    lazy_univ(B, NameB, Arguments),
    !.
term_diff(A, drop_arg(N,Arg), B) :-
    term_type(A, callable),
    term_type(B, callable),
    lazy_univ(A, Name, ArgsA),
    lazy_univ(B, Name, ArgsB),
    nth1(N, ArgsA, Arg, ArgsB),
    !.
term_diff(A, same, A).


term_type(X, Type) :-
    var(X),
    !,
    when(nonvar(X), term_type(X, Type)).
term_type(X, integer) :-
    integer(X),
    !.
term_type(X, float) :-
    float(X),
    !.
term_type(X, callable) :-
    callable(X).

% lazy_univ(Term, Name, Arguments)
%
% True if Term is a callable term with the given Name and Arguments.
% It's just like =../2 but defers computation until Term or Name and
% Arguments is nonvar.
lazy_univ(Term, Name, Args) :-
    var(Term),
    !,
    when(
        ( nonvar(Name), nonvar(Args) ),
        when_proper_list(Args, Term=..[Name|Args])
    ).
lazy_univ(Term, Name, Args) :-
    % nonvar(Term)
    Term =.. [Name|Args].


when_proper_list(List, Goal) :-
    var(List),
    !,
    when(nonvar(List), when_proper_list(List, Goal)).
when_proper_list([], Goal) :-
    call(Goal).
when_proper_list([_|T], Goal) :-
    when_proper_list(T, Goal).
