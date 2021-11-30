:- initialization(main).
:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(verbose, silent).
:- use_module(library(pio)).


%%% configuration, user-defined parameters

calendar_file('lang-calendar.txt').
languages([haskell, python, ruby, rust, scala]).
advent_days(25).

no_repeat_before(2). % min days before repeating a language
max_freq_diff(2). % max difference between most/least used languages at any moment
% max_uses is derived from languages' length %nice: make it configurable?

%%% configuration end


main :-
    read_calendar(PrevLangs),
    draw_next(PrevLangs, NextLang),
    write_to_calendar(PrevLangs, NextLang),
    halt(0).
main :-
    halt(1).

draw_next(PrevLangs, NextLang) :-
    random_lang(NextLang),
    satisfies_constraints(PrevLangs, NextLang),
    append(PrevLangs, [NextLang], NewLangs),
    no_dead_end(NewLangs).

random_lang(Lang) :-
    languages(Langs),
    random_permutation(Langs, Langs2), % matches only once
    member(Lang, Langs2).

satisfies_constraints(PrevLangs, NextLang) :-
    \+ is_recent(PrevLangs, NextLang),
    is_within_max_freq_diff(PrevLangs, NextLang),
    is_within_max_uses(PrevLangs, NextLang).

no_dead_end(Langs) :- % base case
    length(Langs, NDays),
    advent_days(MaxDays),
    NDays is MaxDays, !.
no_dead_end(Langs) :- % recursive case
    draw_next(Langs, _).


%%% constraints

is_recent(PrevLangs, NextLang) :-
    recent(PrevLangs, RecentLangs),
    member(NextLang, RecentLangs).

recent(PrevLangs, RecentLangs) :-
    no_repeat_before(MaxNRecent),
    length(PrevLangs, NPrevLangs),
    NRecent is min(MaxNRecent, NPrevLangs),
    length(RecentLangs, NRecent),
    append(_, RecentLangs, PrevLangs).

is_within_max_freq_diff(PrevLangs, NextLang) :-
    \+ is_above_max_freq_diff(PrevLangs, NextLang).

is_above_max_freq_diff(PrevLangs, NextLang) :-
    max_freq_diff(MaxFreqDiff),
    append(PrevLangs, [NextLang], NewLangs),
    uses_of(NewLangs, NextLang, NextFreq),
    dif(OtherLang, NextLang),
    lang_freq(NewLangs, OtherLang, OtherFreq),
    NextFreq - OtherFreq > MaxFreqDiff.

uses_of(Langs, Lang, NUses) :-
    bagof(Lang-use, member(Lang, Langs), Uses),
    length(Uses, NUses), !.
uses_of(_, _, NUses) :- % bagof fails on no occurrences
    NUses is 0.

lang_freq(Langs, Lang, Freq) :-
    languages(AllLangs),
    member(Lang, AllLangs),
    uses_of(Langs, Lang, Freq).

is_within_max_uses(PrevLangs, NextLang) :-
    max_uses(MaxUses),
    append(PrevLangs, [NextLang], NewLangs),
    uses_of(NewLangs, NextLang, NUses),
    NUses =< MaxUses.

max_uses(N) :-
    advent_days(NDays),
    languages(Langs),
    length(Langs, NLangs),
    N is ceiling(NDays rdiv NLangs).


%%% io

% DCG for lang-calendar.txt
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(_, L), lines(Ls).
line_term()     --> ( `\n` | call(eos) ).
line(Day, Lang) --> `day `, token(Day), `: `, token(Lang), line_term(), !.
token([])     --> [].
token([C|Cs]) --> [C], token(Cs).
eos([], []).

read_calendar(Langs) :-
    calendar_file(CalFile),
    see(CalFile),
    phrase_from_file(lines(LangsChars), CalFile),
    maplist(atom_chars, Langs, LangsChars),
    seen.

line_chars(Day, Lang) --> "day ", token(Day), ": ", token(Lang), "\n".

write_to_calendar(PrevLangs, NextLang) :-
    next_day(PrevLangs, NextDay),
    next_line(NextDay, NextLang, NextLineStr),
    calendar_file(CalFile),
    append(CalFile),
    write(NextLineStr),
    told,
    write(NextLineStr). % echo to stdout

next_day(PrevLangs, NextDay) :-
    length(PrevLangs, CurrentDay),
    NextDay is CurrentDay + 1.

next_line(NextDay, NextLang, NextLineStr) :-
    atom_chars(NextDay, NextDayChars),
    atom_chars(NextLang, NextLangChars),
    phrase(line_chars(NextDayChars, NextLangChars), NextLineChars),
    string_chars(NextLineStr, NextLineChars).
