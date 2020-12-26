:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(pio)).
:- set_prolog_flag(double_quotes, chars).

:- table count_ways_to_reach/2.

:- initialization(main).

main :-
    part1(Part1),
    part2(Part2),
    format('Part 1: ~d~n', [Part1]),
    format('Part 2: ~d~n', [Part2]),
    halt(0).

part1(Answer) :-
    read_file(Nums),
    sort(Nums, Adapters),
    built_in_joltage(Adapters, BI),
    charging_outlet_joltage(CO),
    append(Adapters, [BI], Outputs),
    differences(Outputs, [CO | Adapters], Diffs),
    occurrences_of_term(1, Diffs, Diff1),
    occurrences_of_term(3, Diffs, Diff3),
    Answer #= Diff1 * Diff3.

part2(Answer) :-
    read_file(Nums),
    sort(Nums, Adapters),
    built_in_joltage(Adapters, BI),
    charging_outlet_joltage(CO),
    append(Adapters, [BI], Outputs),
    differences(Outputs, [CO | Adapters], Diffs),
    count_valid_arrangements(Diffs, Answer).

count_valid_arrangements(Diffs, Count) :-
    split_list(Diffs, 3, Ones),
    maplist(length, Ones, Ls),
    exclude(is(0), Ls, Lens),
    maplist(count_ways_to_reach, Lens, Ways),
    foldl(mult, Ways, 1, Count).

mult(X, Y, N) :- N #= X * Y.

%% Counts the ways to reach a given adapter from the previous three adapters
%% This is just a "Tribonacci" sequence
%% Honestly I just got this from https://github.com/MischaDy/PyAdventOfCode2020/blob/main/day%2010/day10_part2.py
count_ways_to_reach(Len, Count) :-
    T #= Len + 2,
    tribonacci(T, Count).

tribonacci(0, 0) :- !.
tribonacci(1, 0) :- !.
tribonacci(2, 1) :- !.
tribonacci(N, Count) :-
    N #> 2,
    N1 #= N - 1,
    N2 #= N - 2,
    N3 #= N - 3,
    tribonacci(N1, C1),
    tribonacci(N2, C2),
    tribonacci(N3, C3),
    Count #= C1 + C2 + C3.

%% Describes a list with the differences between the elements of L1 and L2
differences(L1, L2, Diffs) :-
    maplist(diff, L1, L2, Diffs).
diff(N1, N2, Diff) :-
    Diff #= N1 - N2.

%% Built-in joltage adapter is rated for 3 jolts higher than the
%% highest-rated adapter in your bag.
built_in_joltage(AdapterJoltages, BIJoltage) :-
    max_list(AdapterJoltages, Max),
    BIJoltage #= Max + 3.

charging_outlet_joltage(0).

split_list([], _, [[]]).
split_list([Delim | Xs], Delim, [[] | Splits]) :-
    split_list(Xs, Delim, Splits).
split_list([X | Xs], Delim, [[X | Ys] | Splits]) :-
    dif(X, Delim),
    split_list(Xs, Delim, [Ys | Splits]).

%% Reads the file input.txt into a list of numbers
read_file(Nums) :-
    phrase_from_file(tokens(Ts), 'input.txt'),
    maplist(atom_number, Ts, Nums).

%% DEPRECATED This is way too slow but it works for the Part 2 short example
all_valid_arrangements(Adapters, Arrangements) :-
    setof(Arrangement, valid_arrangement(Adapters, Arrangement), Arrangements).
valid_arrangement([CO | Adapters], [CO | Arrangement]) :-
    sub_set(Adapters, Arrangement),
    last(Adapters, BI),
    last(Arrangement, BI),
    is_valid_arrangement([CO | Arrangement]).
is_valid_arrangement(Adapters) :-
    append(Inputs, [_], Adapters),
    append([_], Outputs, Adapters),
    maplist(can_connect, Inputs, Outputs).
sub_set([], []).
sub_set([_ | T], P) :-
    sub_set(T, P).
sub_set([H | T], [H | P]) :-
    sub_set(T, P).
connections(Adapters, Connections) :-
    maplist(can_connect, Adapters, Connections).
%% Any given adapter can take an input 1, 2, or 3 jolts lower than its
%% rating and still produce its rated output joltage.
can_connect(Input, Output) :-
    Output #= Input + 1
    ; Output #= Input + 3
    ; Output #= Input + 2.

%% DCG definitions for parsing
%% Referenced https://stackoverflow.com/a/40189978

token(T) -->
    digit(L),
    token_(Ls),
    !, % single solution: longest match
    { atom_chars(T, [L|Ls]) }.

digit(A) --> [A], { char_type(A, digit) }.

token_([L|Ls]) --> digit(L), token_(Ls).
token_([])     --> [].

spaces --> [].
spaces --> space, spaces.

space --> [S], { char_type(S, space) }.

tokens([])     --> [].
tokens([T|Ts]) --> token(T), spaces, tokens(Ts).
