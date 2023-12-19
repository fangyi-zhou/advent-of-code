from typing import List


def parse(sequence: str) -> List[str]:
    return sequence.split(",")


def part1(sequence: List[str]) -> int:
    result = 0
    for step in sequence:
        curr = 0
        for ch in step:
            curr = ((curr + ord(ch)) * 17) % 256
            # print(ch, ord(ch), curr)
        result += curr
    return result
