% configuration, user-defined parameters

languages([haskell, python, ruby, rust, scala]).
advent_days(25).
no_repeat_before(2). % min days before repeating a language
max_freq_diff(2). % max difference between most/least used languages at any moment
% max_uses is derived from languages' length %nice: make it configurable?

% configuration end

/*
main :-
    read(PrevLangs),
    draw_next(PrevLangs, NextLang),
    write(NextLang).
*/

draw_next(PrevLangs, NextLang) :-
    random_lang(NextLang),
    satisfies_conditions(PrevLangs, NextLang),
    append(PrevLangs, [NextLang], NewLangs),
    no_dead_end(NewLangs).

random_lang(Lang) :-
    languages(Langs),
    random_permutation(Langs, Langs2), % matches only once
    member(Lang, Langs2).

satisfies_conditions(PrevLangs, NextLang) :-
    \+ is_recent(PrevLangs, NextLang),
    is_within_max_freq_diff(PrevLangs, NextLang),
    is_within_max_uses(PrevLangs, NextLang).

no_dead_end(Langs) :- % base case
    length(Langs, NDays),
    advent_days(MaxDays),
    NDays is MaxDays, !.
no_dead_end(Langs) :- % recursive case
    draw_next(Langs, _).

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
    length(Uses, NUses), !;
    NUses is 0. % bagof fails on no occurrences

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
