from typing import List, Tuple, Optional
from functools import cache


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


@cache
def _fill_blanks(
    pattern: str, expectation: Tuple[int, ...], quota: int, last: Optional[str] = None
) -> int:
    if DEBUG:
        print(last, pattern, quota, expectation)
    if not pattern and all(exp == 0 for exp in expectation):
        if DEBUG:
            print("ok")
        return 1
    if quota == 0:
        pattern = pattern.replace("?", ".")
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
        return _fill_blanks(pattern[1:], expectation[1:], quota, ".")
    if expectation[0] == 0 and pattern[0] == "#":
        return 0
    if expectation[0] > 0 and pattern[0] == "#":
        exp = [expectation[0] - 1] + list(expectation[1:])
        return _fill_blanks(pattern[1:], tuple(exp), quota, "#")
    if pattern[0] == ".":
        if last == "#" and expectation[0] > 0:
            return 0
        return _fill_blanks(pattern[1:], expectation, quota, ".")
    if pattern[0] == "?":
        result = 0
        if expectation[0] > 0:
            exp = [expectation[0] - 1] + list(expectation[1:])
            result += _fill_blanks(pattern[1:], tuple(exp), quota - 1, "#")
            if last == "#":
                return result
        if expectation[0] == 0:
            result += _fill_blanks(pattern[1:], expectation[1:], quota, ".")
        else:
            result += _fill_blanks(pattern[1:], expectation, quota, ".")
        return result
    return 0


def part1(rows: List[Tuple[str, List[int]]]) -> int:
    arrangements = 0
    for pattern, summary in rows:
        known_damages = pattern.count("#")
        unknown_damages = sum(summary) - known_damages
        result = _fill_blanks(pattern, tuple(summary), unknown_damages)
        arrangements += result
        # old_result = 0
        # replacements = "." * unknown_operationals + "#" * unknown_damages
        # for replacement in multiset_permutations(replacements):
        #     if _summarise(pattern, replacement) == summary:
        #         old_result += 1
        # if old_result != new_result:
        #     print(pattern, summary, old_result, new_result)
    return arrangements


def part2(rows: List[Tuple[str, List[int]]]) -> int:
    arrangements = 0
    for pattern, summary in rows:
        known_damages = pattern.count("#")
        unknown_damages = sum(summary) - known_damages
        pattern = "?".join([pattern] * 5)
        summary = sum([summary] * 5, [])
        result = _fill_blanks(pattern, tuple(summary), unknown_damages * 5)
        arrangements += result
    return arrangements
