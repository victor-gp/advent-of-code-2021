# https://adventofcode.com/2021/day/3

from sys import stderr

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
        return "0" if count < len(binary_codes) / 2 else "1"
    gamma_bits = map(most_common, counts)
    return "".join(gamma_bits)

def epsilon_rate_bin(binary_codes: list[str]) -> str:
    gamma_bin = gamma_rate_bin(binary_codes)
    epsilon_bits = map(negate, gamma_bin)
    return "".join(epsilon_bits)

def negate(bit: str) -> str:
    return "0" if bit == "1" else "1"

gamma_bin= gamma_rate_bin(binary_codes)
print(f"gamma rate binary: {gamma_bin}", file = stderr)
epsilon_bin = epsilon_rate_bin(binary_codes)
print(f"epsilon rate binary: {epsilon_bin}", file = stderr)

gamma_rate = int(gamma_bin, 2)
epsilon_rate = int(epsilon_bin, 2)
power_consumption = gamma_rate * epsilon_rate

print(power_consumption)
