from typing import List, Tuple
from sympy.utilities.iterables import multiset_permutations


def parse(lines: str) -> List[Tuple[str, List[int]]]:
    def parse_line(line: str) -> Tuple[str, List[int]]:
        pattern, summary = line.split()
        summaries = [*map(int, summary.split(","))]
        return pattern, summaries

    return [*map(parse_line, lines.split("\n"))]


def _summarise(pattern: str, replacement: List[str]) -> List[int]:
    count = 0
    counts = []
    replacement_idx = 0
    for ch in pattern:
        if ch == "?":
            ch = replacement[replacement_idx]
            replacement_idx += 1
        if ch == "." and count:
            counts.append(count)
            count = 0
        elif ch == "#":
            count += 1
    if count:
        counts.append(count)
    return counts


def part1(rows: List[Tuple[str, List[int]]]) -> int:
    arrangements = 0
    for pattern, summary in rows:
        unknowns = pattern.count("?")
        known_damages = pattern.count("#")
        unknown_damages = sum(summary) - known_damages
        unknown_operationals = unknowns - unknown_damages
        replacements = "." * unknown_operationals + "#" * unknown_damages
        for replacement in multiset_permutations(replacements):
            if _summarise(pattern, replacement) == summary:
                arrangements += 1
    return arrangements
