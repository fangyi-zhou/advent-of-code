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


DEBUG = False


def _fill_blanks(pattern: str, expectation: List[int], memory: str = "") -> int:
    if DEBUG:
        print(memory, pattern, expectation)
    if not pattern and all(exp == 0 for exp in expectation):
        if DEBUG:
            print("ok")
        return 1
    if pattern and not expectation:
        if all(ch != "#" for ch in pattern):
            if DEBUG:
                print("ok")
            return 1
        else:
            return 0
    if not pattern and expectation:
        return 0
    if expectation[0] == 0 and pattern[0] == ".":
        return _fill_blanks(pattern[1:], expectation[1:], memory + ".")
    if expectation[0] == 0 and pattern[0] == "#":
        return 0
    if expectation[0] > 0 and pattern[0] == "#":
        return _fill_blanks(
            pattern[1:], [expectation[0] - 1] + expectation[1:], memory + "#"
        )
    if pattern[0] == ".":
        if memory and memory[-1] == "#" and expectation[0] > 0:
            return 0
        return _fill_blanks(pattern[1:], expectation, memory + ".")
    if pattern[0] == "?":
        result = 0
        if expectation[0] > 0:
            result += _fill_blanks(
                pattern[1:], [expectation[0] - 1] + expectation[1:], memory + "#"
            )
            if memory and memory[-1] == "#":
                return result
        if expectation[0] == 0:
            result += _fill_blanks(pattern[1:], expectation[1:], memory + ".")
        else:
            result += _fill_blanks(pattern[1:], expectation, memory + ".")
        return result
    return 0


def part1(rows: List[Tuple[str, List[int]]]) -> int:
    arrangements = 0
    for pattern, summary in rows:
        result = _fill_blanks(pattern, summary)
        arrangements += result
        # old_result = 0
        # unknowns = pattern.count("?")
        # known_damages = pattern.count("#")
        # unknown_damages = sum(summary) - known_damages
        # unknown_operationals = unknowns - unknown_damages
        # replacements = "." * unknown_operationals + "#" * unknown_damages
        # for replacement in multiset_permutations(replacements):
        #     if _summarise(pattern, replacement) == summary:
        #         old_result += 1
        # if old_result != new_result:
        #     print(pattern, summary, old_result, new_result)
    return arrangements


# def part2(rows: List[Tuple[str, List[int]]]) -> int:
#     arrangements = 0
#     for pattern, summary in rows:
#         pattern = "?".join([pattern] * 5)
#         summary = sum([summary] * 5, [])
#         result = _fill_blanks(pattern, summary)
#         arrangements += result
#     return arrangements
