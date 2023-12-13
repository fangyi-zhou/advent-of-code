from typing import List
from itertools import groupby


def parse(inputs: str) -> List[List[str]]:
    lines = inputs.split("\n")
    return [list(g) for k, g in groupby(lines, lambda k: k != "") if k]


def find_mirror_with_smudge(pattern: List[str], smudge: int) -> int:
    for j in range(1, len(pattern[0])):
        cols = min(j, len(pattern[0]) - j)
        if (
            sum(
                pattern[i][j - j_ - 1] != pattern[i][j + j_]
                for i in range(len(pattern))
                for j_ in range(cols)
            )
            == smudge
        ):
            return j
    for i in range(1, len(pattern)):
        rows = min(i, len(pattern) - i)
        if (
            sum(
                pattern[i - i_ - 1][j] != pattern[i + i_][j]
                for i_ in range(rows)
                for j in range(len(pattern[0]))
            )
            == smudge
        ):
            return 100 * i
    raise RuntimeError("unreachable")


def part1(inputs: List[List[str]]) -> int:
    return sum(map(lambda p: find_mirror_with_smudge(p, 0), inputs))


def part2(inputs: List[List[str]]) -> int:
    return sum(map(lambda p: find_mirror_with_smudge(p, 1), inputs))
