from typing import List, Tuple
from functools import reduce


def parse(lines: str) -> List[Tuple[int, int]]:
    times_str, distances_str = lines.split("\n")
    times = map(int, times_str.split()[1:])
    distances = map(int, distances_str.split()[1:])
    return [*zip(times, distances)]


def part1(races: List[Tuple[int, int]]) -> int:
    def ways(race: Tuple[int, int]) -> int:
        time, distance = race
        count = 0
        for i in range(time):
            if i * (time - i) > distance:
                count += 1
        return count

    return reduce(lambda x, y: x * y, map(ways, races), 1)
