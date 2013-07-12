:- module(term_diff, [term_diff/3]).
:- use_module(library(lcs), [lcs/3, lcs/5]).
:- use_module(library(when), [when/2]).

term_diff(A, name(NameA,NameB), B) :-
    lazy_callable(A),
    lazy_callable(B),
    dif(NameA, NameB),
    lazy_univ(A, NameA, Arguments),
    lazy_univ(B, NameB, Arguments),
    !.
term_diff(A, drop_arg(N,Arg), B) :-
    term_nth1(A, N, Arg, B),
    !.
term_diff(A, add_arg(N,Arg), B) :-
    term_nth1(B, N, Arg, A),
    !.
term_diff(A, [], A) :-
    !.
term_diff(A, Diffs, B) :-
    % defer patch application until we have a target
    nonvar(Diffs),
    var(A),
    var(B),
    !,
    when((nonvar(A);nonvar(B)), term_diff(A,Diffs,B)).
term_diff(A, Diffs, B) :-
    % apply a list of diffs
    nonvar(Diffs),
    Diffs = [Diff|Rest],
    !,
    term_diff(A, Diff, Z),
    term_diff(Z, Rest, B).
term_diff(A, Diffs, B) :-
    % calculate a list of diffs
    var(Diffs),
    callable(A),
    callable(B),
    A =.. ListA,
    B =.. ListB,
    lcs(ListA, ListB, LCS),
    LCS \= [],
    !,
    lcs_diff(LCS, 0, ListA, ListB, Diffs).
term_diff(A, alter(A,B), B) :-
    dif(A,B).

% construct a diff for two lists given one of their longest common
% subsequences
lcs_diff([], _, [], [], []) :-
    % left and right sides empty
    !.
lcs_diff([], N0, [], [H|T], [add_arg(N0,H)|Diffs]) :-
    % nothing in common; left side empty
    succ(N0, N),
    lcs_diff([], N, [], T, Diffs),
    !.
lcs_diff([], N, [H|T], Right, [drop_arg(N,H)|Diffs]) :-
    % nothing in common; left not empty
    lcs_diff([], N, T, Right, Diffs).
lcs_diff([X|LCS], N0, [X|Left], [X|Right], Diffs) :-
    % left and right start with common element
    !,
    succ(N0, N),
    lcs_diff(LCS, N, Left, Right, Diffs).
lcs_diff([X|LCS], N, [L|Left], Right, [drop_arg(N,L)|Diffs]) :-
    % common element is not on the left
    dif(X, L),
    !,
    lcs_diff([X|LCS], N, Left, Right, Diffs).
lcs_diff([X|LCS], N0, [X|Left], [R|Right], [add_arg(N0,R)|Diffs]) :-
    % common element is on the left, but not the right
    dif(X, R),
    succ(N0, N),
    lcs_diff([X|LCS], N, [X|Left], Right, Diffs).


% generalization for drop_arg/2 and add_arg/2 patches
term_nth1(A, N, Arg, B) :-
    lazy_callable(A),
    lazy_callable(B),
    lazy_univ(A, Name, ArgsA),
    lazy_univ(B, Name, ArgsB),
    nth1(N, ArgsA, Arg, ArgsB).


lazy_callable(X) :-
    when(nonvar(X), callable(X)).


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
