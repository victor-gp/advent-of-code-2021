# https://adventofcode.com/2021/day/6

from collections import deque
from sys import stderr, argv


def main(part: int, input: str) -> None:
    ntimer_values = 9  # internal timer values go from 0 to 8
    nfish_at_timers = [0] * ntimer_values
    timers = [int(timer_str) for timer_str in input.split(",")]
    for timer in timers:
        nfish_at_timers[timer] += 1
    nfish_at_timers = deque(nfish_at_timers, ntimer_values)
    if part == 1:
        simulate(nfish_at_timers, 80)
    else:
        simulate(nfish_at_timers, 256)
    print(nfish_at_timers, file=stderr)
    total = sum(nfish_at_timers)
    print(total)


# mutates nfish_at_timers
def simulate(nfish_at_timers: deque[int], days_left: int) -> None:
    if days_left == 0:
        return
    # decrease timers, add new lanternfish (timer = 8)
    nfish_at_timers.rotate(-1)
    # reset the timer of lanternfish after creation
    nfish_at_timers[6] += nfish_at_timers[8]
    simulate(nfish_at_timers, days_left - 1)


bad_argc = 'Santa says: "this program uses a single argument. we give out stars on channels 1 and 2."'
bad_arg1 = 'Santa says: "there\'s nothing here, you may want to try on channels 1 or 2."'

if len(argv) != 2:  # 0 is prog_name
    print(bad_argc, file=stderr)
    exit(2)
elif int(argv[1]) not in [1, 2]:
    print(bad_arg1, file=stderr)
    exit(2)

part = int(argv[1])

with open("../../input/day06-in.txt") as f:
    input = f.read()

main(part, input)
