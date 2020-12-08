:- use_module(library(pio)).
:- set_prolog_flag(double_quotes, chars).

part1(Answer) :-
    read_file(Numbers),
    sums_to_2020(Numbers, N1, N2),
    Answer is N1 * N2.

part2(Answer) :-
    read_file(Numbers),
    sums_to_2020(Numbers, N1, N2, N3),
    Answer is N1 * N2 * N3.

% Numbers is a list of numbers.
% Returns the numbers that sum to 2020 (Nx)
sums_to_2020(Numbers, N1, N2) :-
    select(N1, Numbers, Rest),
    member(N2, Rest),
    2020 is N1 + N2.

sums_to_2020(Numbers, N1, N2, N3) :-
    select(N1, Numbers, Rest),
    member(N2, Rest),
    member(N3, Rest),
    2020 is N1 + N2 + N3.

%% Reads the file input.txt into a list of numbers
read_file(Numbers) :-
    phrase_from_file(tokens(Ts), 'input.txt'),
    phrase(data(Numbers), Ts).

%% DCG definitions for parsing
%% Referenced https://stackoverflow.com/a/40189978

token(T) -->
        digit(L),
        token_(Ls),
        !, % single solution: longest match
        { atom_chars(T, [L|Ls]) }.

digit(A) --> [A], { char_type(A, digit) }.

token_([])     --> [].
token_([L|Ls]) --> digit(L), token_(Ls).

spaces --> [].
spaces --> space, spaces.

space --> [S], { char_type(S, space) }.

% Returns a list of tokens
% e.g. Ts = ['1028', '1987', '1938', '1136', |...] 
tokens([])     --> [].
tokens([T|Ts]) --> token(T), spaces, tokens(Ts).

data([])     --> [].
data([D|Ds]) --> data_(D), data(Ds).

data_(N) --> [A], { atom_number(A, N) }.
