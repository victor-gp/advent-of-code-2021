# https://adventofcode.com/2021/day/3

from sys import stderr, argv
from copy import deepcopy
from typing import TypeAlias, Callable

with open("../../input/day03-in.txt") as f:
    input: str = f.read()
binary_codes = input.splitlines()


def gamma_rate_bin(binary_codes: list[str]) -> str:
    bit_len = len(binary_codes[0])
    counts = [0] * bit_len
    for binary_code in binary_codes:
        bits_i = [int(bit_s) for bit_s in binary_code]
        counts = [sum(i) for i in zip(counts, bits_i)]
    def most_common(count: int) -> str:
        # in case of tie, criterion bit is 1 for oxygen_gen
        # epsilon_rate_bin negates that, which just suits co2_scrub
        return "0" if count < len(binary_codes) / 2 else "1"
    gamma_bits = map(most_common, counts)
    return "".join(gamma_bits)


def epsilon_rate_bin(binary_codes: list[str]) -> str:
    gamma_bin = gamma_rate_bin(binary_codes)
    epsilon_bits = map(negate, gamma_bin)
    return "".join(epsilon_bits)

def negate(bit: str) -> str:
    return "0" if bit == "1" else "1"


def part1(binary_codes: list[str]) -> None:
    gamma_bin = gamma_rate_bin(binary_codes)
    print(f"gamma rate binary: {gamma_bin}", file=stderr)
    epsilon_bin = epsilon_rate_bin(binary_codes)
    print(f"epsilon rate binary: {epsilon_bin}", file=stderr)

    gamma_rate = int(gamma_bin, 2)
    epsilon_rate = int(epsilon_bin, 2)
    power_consumption = gamma_rate * epsilon_rate
    print(power_consumption)


criteria_fn: TypeAlias = Callable[[list[str], int], str]

def find_value(bit_criteria: criteria_fn, binary_codes: list[str]) -> str:
    binary_codes = deepcopy(binary_codes)
    bit_len = len(binary_codes[0])
    for i in range(bit_len):
        criterion_bit = bit_criteria(binary_codes, i)
        binary_codes = list(filter(
            lambda bin_code: bin_code[i] == criterion_bit,
            binary_codes
        ))
        if len(binary_codes) == 1:
            return binary_codes[0]
    raise Exception("not found")


def oxygen_gen_criteria(binary_codes: list[str], i: int) -> str:
    return gamma_rate_bin(binary_codes)[i]

def co2_scrub_criteria(binary_codes: list[str], i: int) -> str:
    return epsilon_rate_bin(binary_codes)[i]


def part2(binary_codes: list[str]) -> None:
    oxygen_bin = find_value(oxygen_gen_criteria, binary_codes)
    print(oxygen_bin, file=stderr)
    co2_bin = find_value(co2_scrub_criteria, binary_codes)
    print(co2_bin, file=stderr)

    oxygen_generator_rating = int(oxygen_bin, 2)
    co2_scrubber_rating = int(co2_bin, 2)
    life_support_rating = oxygen_generator_rating * co2_scrubber_rating
    print(life_support_rating)


bad_argc = 'Santa says: "this program uses a single argument. we give out stars on channels 1 and 2."'
bad_arg1 = 'Santa says: "there\'s nothing here, you may want to try on channels 1 or 2."'

if len(argv) != 2:  # 0 is prog_name
    print(bad_argc, file=stderr)
    exit(2)
elif int(argv[1]) == 1:
    part1(binary_codes)
elif int(argv[1]) == 2:
    part2(binary_codes)
else:
    print(bad_arg1, file=stderr)
    exit(2)
