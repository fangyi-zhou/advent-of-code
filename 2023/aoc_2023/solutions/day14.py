import copy
from typing import List, Set, Dict


def parse(lines: str) -> List[List[str]]:
    return [*map(lambda s: list(s), lines.split("\n"))]


def _score(lines: List[List[str]]) -> int:
    score = 0
    for j in range(len(lines[0])):
        for i in range(len(lines)):
            if lines[i][j] == "O":
                score += len(lines) - i
    return score


def _spin(lines: List[List[str]]) -> List[List[str]]:
    for j in range(len(lines[0])):
        last_wall = -1
        rocks = 0
        for i in range(len(lines)):
            if lines[i][j] == "O":
                rocks += 1
                lines[i][j] = "."
            elif lines[i][j] == "#":
                if rocks:
                    for r in range(rocks):
                        lines[last_wall + r + 1][j] = "O"
                last_wall = i
                rocks = 0
        if rocks:
            for r in range(rocks):
                lines[last_wall + r + 1][j] = "O"
    return lines


def part1(lines: List[List[str]]) -> int:
    return _score(_spin(lines))


def part2(lines: List[List[str]]) -> int:
    seen: Dict[str, int] = dict()
    cycle_count = 0
    while True:
        cycle_count += 1
        for _ in range(4):
            lines = _spin(lines)
            # https://stackoverflow.com/questions/8421337/rotating-a-two-dimensional-array-in-python
            lines = [*map(lambda l: list(l), zip(*lines[::-1]))]
        stringified = "\n".join("".join(line) for line in lines)
        if stringified in seen:
            cycle_start = seen[stringified]
            cycle_length = cycle_count - cycle_start
            break
        seen[stringified] = cycle_count
    seen_idx = (1000000000 - cycle_start) % cycle_length + cycle_start
    for stringified, cycle_count in seen.items():
        if cycle_count == seen_idx:
            return _score(parse(stringified))
    raise RuntimeError("Unreachable")
