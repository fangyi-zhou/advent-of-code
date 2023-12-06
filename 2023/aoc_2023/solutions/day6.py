from functools import reduce
from typing import List, Tuple
import math


def parse(lines: str) -> Tuple[List[int], List[int]]:
    times_str, distances_str = lines.split("\n")
    times = map(int, times_str.split()[1:])
    distances = map(int, distances_str.split()[1:])
    return [*times], [*distances]


def _total_ways(race: Tuple[int, int]) -> int:
    time, distance = race
    count = 0
    # Find x such that x * (time - x) > distance
    # x^2 - time * x + distance < 0
    delta = time * time - 4 * distance
    x_min = math.ceil((time - math.sqrt(delta)) / 2.0)
    x_max = math.floor((time + math.sqrt(delta)) / 2.0)
    # Handle boundary cases
    if x_min * x_min - time * x_min + distance == 0:
        x_min += 1
    if x_max * x_max - time * x_max + distance == 0:
        x_max -= 1
    return x_max - x_min + 1


def part1(inputs: Tuple[List[int], List[int]]) -> int:
    races = zip(*inputs)
    return reduce(lambda x, y: x * y, map(_total_ways, races), 1)


def part2(races: Tuple[List[int], List[int]]) -> int:
    times, distances = races
    time = int("".join(map(str, times)))
    distance = int("".join(map(str, distances)))
    return _total_ways((time, distance))
