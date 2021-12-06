# https://adventofcode.com/2021/day/6

from collections import deque
from sys import stderr


def main(input: str) -> None:
    ntimer_values = 9  # internal timer values go from 0 to 8
    nfish_at_timers = [0] * ntimer_values
    timers = [int(timer_str) for timer_str in input.split(",")]
    for timer in timers:
        nfish_at_timers[timer] += 1
    nfish_at_timers = deque(nfish_at_timers, ntimer_values)
    simulate(nfish_at_timers, 80)
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


with open("../../input/day06-in.txt") as f:
    input = f.read()

main(input)
