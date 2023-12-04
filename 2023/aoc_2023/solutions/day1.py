from typing import List, Generator


def parse(input: str) -> List[str]:
    return input.split("\n")


def part1(lines: List[str]) -> int:
    def part1_solve(line: str) -> int:
        digits = [ord(ch[0]) - ord("0") for ch in line if ch >= "0" and ch <= "9"]
        return digits[0] * 10 + digits[-1]

    return sum(map(part1_solve, lines))


DIGITS_MAP = {
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}


def part2(lines: List[str]) -> int:
    def gen_digits(line: str) -> Generator[int, None, None]:
        while line:
            matched = False
            if line[0] >= "0" and line[0] <= "9":
                digit = ord(line[0]) - ord("0")
                matched = True
                yield digit
            if not matched:
                for word, digit in DIGITS_MAP.items():
                    if line.startswith(word):
                        matched = True
                        yield digit
                        break
            line = line[1:]

    def part2_solve(line: str) -> int:
        digits = [*gen_digits(line)]
        print(digits)
        return digits[0] * 10 + digits[-1]

    return sum(map(part2_solve, lines))
