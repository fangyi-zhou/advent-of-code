from dataclasses import dataclass
from typing import List, Tuple


@dataclass
class RangeEntry:
    destination: int
    source: int
    length: int


class TransformationV2:
    break_points: List[Tuple[int, int]]
    inv_break_points: List[Tuple[int, int]]

    def __init__(self, entries: List[RangeEntry]):
        entries = sorted(entries, key=lambda entry: entry.source)
        break_points: List[Tuple[int, int]] = []
        last_end = 0
        for entry in entries:
            if last_end != entry.source:
                break_points.append((last_end, 0))
            diff = entry.destination - entry.source
            if not break_points or diff != break_points[-1][1]:
                break_points.append((entry.source, diff))
            last_end = entry.source + entry.length
        break_points.append((last_end, 0))
        self.break_points = break_points

    def __str__(self):
        return f"TransformationV2(break_points={str(self.break_points)})"

    def apply(self, src: int) -> int:
        last_diff = 0
        for break_point in self.break_points:
            start, diff = break_point
            if start > src:
                return src + last_diff
            last_diff = diff
        return src + last_diff


@dataclass
class Almanac:
    seeds: List[int]
    maps: List[TransformationV2]

    def apply_all_transformations(self, src: int) -> int:
        for transformation in self.maps:
            src = transformation.apply(src)
        return src


def parse(input: str) -> Almanac:
    lines = input.strip().split("\n")
    seeds_line = lines[0]
    _, seeds_str = seeds_line.split(":")
    seeds = [*map(int, seeds_str.strip().split())]
    name = ""
    range_entries_buffer: List[RangeEntry] = []
    transformations: List[TransformationV2] = []
    for line in lines[2:]:
        if "map" in line:
            name = line.split()[0]
            continue
        if not line:
            transformations.append(TransformationV2(entries=range_entries_buffer))
            range_entries_buffer = []
            continue
        destination, source, length = [*map(int, line.split())]
        range_entries_buffer.append(
            RangeEntry(destination=destination, source=source, length=length)
        )
    transformations.append(TransformationV2(entries=range_entries_buffer))
    return Almanac(seeds=seeds, maps=transformations)


def part1(almanac: Almanac) -> int:
    print(*map(lambda src: almanac.apply_all_transformations(src), almanac.seeds))
    return min(map(lambda src: almanac.apply_all_transformations(src), almanac.seeds))
