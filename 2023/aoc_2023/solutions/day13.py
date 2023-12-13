from typing import List
from itertools import groupby


def parse(inputs: str) -> List[List[str]]:
    lines = inputs.split("\n")
    return [list(g) for k, g in groupby(lines, lambda k: k != "") if k]


def part1(inputs: List[List[str]]) -> int:
    def find_mirror(pattern: List[str]) -> int:
        for j in range(1, len(pattern[0])):
            cols = min(j, len(pattern[0]) - j)
            if all(
                pattern[i][j - j_ - 1] == pattern[i][j + j_]
                for i in range(len(pattern))
                for j_ in range(cols)
            ):
                return j
        for i in range(1, len(pattern)):
            rows = min(i, len(pattern) - i)
            if all(
                pattern[i - i_ - 1][j] == pattern[i + i_][j]
                for i_ in range(rows)
                for j in range(len(pattern[0]))
            ):
                return 100 * i
        raise RuntimeError("unreachable")

    return sum(map(find_mirror, inputs))
