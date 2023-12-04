from typing import List


def parse(input: str) -> List[str]:
    return input.split("\n")


def part1(lines: List[str]) -> int:
    def part1_solve(line: str) -> int:
        digits = [ord(ch[0]) - ord("0") for ch in line if ch >= "0" and ch <= "9"]
        return digits[0] * 10 + digits[-1]

    return sum(map(part1_solve, lines))
