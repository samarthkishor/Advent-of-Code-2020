:- use_module(library(pio)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, chars).

:- initialization(main).

main :-
    part1(Part1),
    part2(Part2),
    format('Part 1: ~d~n', [Part1]),
    format('Part 2: ~d~n', [Part2]),
    halt(0).

part1(Answer) :-
    read_file(Nums),
    first_invalid(25, Nums, Answer),
    !.

%% TODO optimize this because it's really slow
part2(Answer) :-
    read_file(Nums),
    part1(Invalid),
    find_range(Nums, Range),
    sum_list(Range, Invalid),
    min_list(Range, Min),
    max_list(Range, Max),
    Answer is Min + Max,
    !.

find_range(Nums, Range) :-
    append([_, Range, _], Nums),
    length(Range, Len),
    Len >= 2.

first_invalid(_, [], _).
first_invalid(PreambleLength, Nums, Invalid) :-
    preamble(PreambleLength, Nums, Preamble),
    append(Preamble, [Invalid | _], Nums),
    not(valid(Preamble, Invalid)).
first_invalid(PreambleLength, [_ | Ns], Invalid) :-
    first_invalid(PreambleLength, Ns, Invalid).

%% True if N is the sum of any two elements in the Preamble list
valid(Preamble, N) :-
    member(X, Preamble),
    member(Y, Preamble),
    X \= Y,
    N is X + Y.

%% Takes the first N elements of the list
%% preamble(+N, +Nums, -Preamble)
preamble(N, Nums, Preamble) :-
    prefix(Preamble, Nums),
    length(Preamble, N).

%% Reads the file input.txt into a list of numbers
read_file(Nums) :-
    phrase_from_file(tokens(Ts), 'input.txt'),
    maplist(atom_number, Ts, Nums).

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
