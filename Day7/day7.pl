:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- set_prolog_flag(double_quotes, chars).

:- consult(parsed).
:- initialization(main).

:- dynamic contains/2.

main :-
    part1(Part1),
    part2(Part2),
    format('Part 1: ~d~n', [Part1]),
    format('Part 2: ~d~n', [Part2]),
    halt(0).

part1(Answer) :-
    retractall(contains(_, _)),
    parse(Data),
    maplist(call, Data),
    contains_target('shiny gold', List),
    length(List, Answer).

part2(Answer) :-
    total_bags([{1, 'shiny gold'}], N),
    Answer is N - 1.

total_bags([], 0).
total_bags([{Count, Color} | Bags], Total) :-
    contains_bags({Count, Color}, ContainedBags),
    total_bags(ContainedBags, TotalContainedBags),
    total_bags(Bags, BFS_Result),
    Total #= Count * (TotalContainedBags + 1) + BFS_Result.  % count the bag itself

%% Binds Bags to the list of bags that Target color must directly contain
contains_bags({_, Target}, Bags) :-
    findall({N, C}, contains(Target, {N, C}), Bs),
    sort(Bs, Bags).

%% Binds Colors to a list of the bag colors that can contain the target color
contains_target(Target, Colors) :-
    findall(Color, can_contain(Color, Target), Cs),
    sort(Cs, Colors).

can_contain(Color, Target) :-
    contains(Color, {_, Target}).
can_contain(Color, Target) :-
    contains(Color, {_, C}),
    can_contain(C, Target).

%% Terrible name but it works
baggins(_, []).
baggins(Color, [{Num, HasColor} | Rest]) :-
    assertz(contains(Color, {Num, HasColor})),
    baggins(Color, Rest).

%% DCG definitions for parsing
%% TODO fix this eventually

sentences([S|Ss]) --> sentence(S), ".", blanks, sentences(Ss).
sentences([S])    --> sentence(S).

sentence(baggins(Color, Bags)) -->
        bag(Color), bag_list(Bags).
sentence(baggins(Color, [{0, 'no other'}])) -->
        bag(Color), "no other bags".

bag(Color) -->
        string(Codes), "bags contain ",
        { atom_codes(Color, Codes) }.

bag_list([NumCol|Rest]) -->  num_color(NumCol), ",", white, bag_list(Rest).
bag_list([NumCol])      -->  num_color(NumCol).

num_color({N, C}) -->
         number(N), whites, string(Codes), ("bag" | "bags"),
        { atom_codes(C, Codes) }.

newline --> [S], { char_type(S, newline) }.
