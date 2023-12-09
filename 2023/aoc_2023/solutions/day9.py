from typing import List

from scipy import interpolate


def parse(lines: str) -> List[List[int]]:
    return [*map(lambda line: [*map(int, line.split())], lines.split("\n"))]


def part1(sequences: List[List[int]]) -> int:
    def process_sequence(sequence: List[int]) -> int:
        # Not using Lagrange due to numerical instability
        f = interpolate.BarycentricInterpolator(*zip(*enumerate(sequence)))
        next_value = f(len(sequence))
        return round(next_value.item())

    return sum(map(process_sequence, sequences))


def part2(sequences: List[List[int]]) -> int:
    def process_sequence(sequence: List[int]) -> int:
        # Not using Lagrange due to numerical instability
        f = interpolate.BarycentricInterpolator(*zip(*enumerate(sequence)))
        next_value = f(-1)
        return round(next_value.item())

    return sum(map(process_sequence, sequences))
