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

draw_next(PrevLangs, NextLang) :-
    random_lang(NextLang),
    satisfies_conditions(PrevLangs, NextLang),
    append(PrevLangs, [NextLang], NewLangs),
    \+ dead_end(NewLangs).
*/

random_lang(Lang) :-
    languages(Langs),
    random_permutation(Langs, Langs2), % matches only once
    member(Lang, Langs2).

/*
satisfies_conditions(PrevLangs, NextLang) :-
    \+ is_recent(nextLang),
    append(PrevLangs, [NextLang], NewLangs),
    is_within_max_freq_diff(),
    is_within_max_uses().

dead_end(Langs) :-
    length(Langs, NDays),
    advent_days(MaxDays),
    NDays is MaxDays; draw_next(Langs, _).
*/

is_recent(Lang, PrevLangs) :-
    recent(PrevLangs, RecentLangs),
    member(Lang, RecentLangs).

recent(PrevLangs, RecentLangs) :-
    no_repeat_before(MaxRecent),
    length(PrevLangs, NPrevLangs),
    NRecent is min(MaxRecent, NPrevLangs),
    append(_, RecentLangs, PrevLangs),
    length(RecentLangs, NRecent).

max_uses(N) :-
    advent_days(NDays),
    languages(Langs),
    length(Langs, NLangs),
    N is ceiling(NDays rdiv NLangs).
