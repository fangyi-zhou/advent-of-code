from __future__ import annotations

from typing import List, Tuple
import bisect

TData = List[Tuple[int, int]]
TPart1 = int


def parse(input_data: str) -> TData:
    # input is a single line (or possibly with newlines) containing ranges like "11-22,95-115,..."
    s = input_data.strip().replace('\n', '')
    if not s:
        return []
    parts = [p for p in s.split(',') if p]
    ranges: TData = []
    for part in parts:
        a_str, b_str = part.split('-')
        a = int(a_str)
        b = int(b_str)
        ranges.append((a, b))
    return ranges


def _generate_repeated_numbers_up_to(max_value: int) -> List[int]:
    # generate all numbers that are two copies of the same digit-sequence (no leading zeros)
    if max_value < 11:
        return []
    result: List[int] = []
    max_len = len(str(max_value))
    # total length must be even: 2*k
    for k in range(1, max_len // 2 + 1):
        pow10_k = 10 ** k
        start = 10 ** (k - 1)
        end = 10 ** k - 1
        for left in range(start, end + 1):
            n = left * (pow10_k) + left
            if n > max_value:
                break
            result.append(n)
    return result


def part1(data: TData) -> TPart1:
    if not data:
        return 0
    max_b = max(b for (_, b) in data)
    repeated = _generate_repeated_numbers_up_to(max_b)
    repeated.sort()
    total = 0
    for a, b in data:
        # find slice of repeated within [a,b]
        lo = bisect.bisect_left(repeated, a)
        hi = bisect.bisect_right(repeated, b)
        if lo < hi:
            total += sum(repeated[lo:hi])
    return total

# Part 2: repeated at least twice (any number of repeats)
def _generate_repeated_numbers_any_up_to(max_value: int) -> List[int]:
    if max_value < 11:
        return []
    result = set()
    max_digits = len(str(max_value))
    for base_len in range(1, max_digits):
        start = 10 ** (base_len - 1)
        end = 10 ** base_len - 1
        max_repeats = max_digits // base_len
        for repeats in range(2, max_repeats + 1):
            for base in range(start, end + 1):
                s = str(base) * repeats
                n = int(s)
                if n > max_value:
                    break
                result.add(n)
    return sorted(result)

def part2(data: TData) -> int:
    if not data:
        return 0
    max_b = max(b for (_, b) in data)
    repeated_any = _generate_repeated_numbers_any_up_to(max_b)
    repeated_any.sort()
    total2 = 0
    for a, b in data:
        lo = bisect.bisect_left(repeated_any, a)
        hi = bisect.bisect_right(repeated_any, b)
        if lo < hi:
            total2 += sum(repeated_any[lo:hi])
    return total2

if __name__ == '__main__':
    # Example from the prompt
    example = (
        "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
        "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
        "824824821-824824827,2121212118-2121212124"
    )
    data = parse(example)
    ans = part1(data)
    assert ans == 1227775554, f"example mismatch: {ans}"
    print('part1 example:', ans)


    ans2 = part2(data)
    assert ans2 == 4174379265, f"part2 example mismatch: {ans2}"
    print('part2 example:', ans2)
