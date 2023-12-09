from dataclasses import dataclass
from typing import List, Tuple
from itertools import batched


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
        return self.apply_range((src, 1))[0][0]

    def apply_range(self, src_range: Tuple[int, int]) -> List[Tuple[int, int]]:
        src_start, src_length = src_range
        last_diff = 0
        dest_ranges = []
        for break_point in self.break_points:
            start, diff = break_point
            if start > src_start:
                range_length = min(start - src_start, src_length)
                dest_ranges.append((src_start + last_diff, range_length))
                src_length -= range_length
                src_start = start
                if not src_length:
                    return dest_ranges
            last_diff = diff
        if src_length:
            dest_ranges.append((src_start + last_diff, src_length))
        return dest_ranges


@dataclass
class Almanac:
    seeds: List[int]
    maps: List[TransformationV2]

    def apply_all_transformations(self, src: int) -> int:
        for transformation in self.maps:
            src = transformation.apply(src)
        return src

    def apply_all_transformations_range(
        self, src_range: Tuple[int, int]
    ) -> List[Tuple[int, int]]:
        ranges = [src_range]
        for transformation in self.maps:
            ranges = sum([*map(lambda r: transformation.apply_range(r), ranges)], [])
        return ranges


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
    return min(map(lambda src: almanac.apply_all_transformations(src), almanac.seeds))


def part2(almanac: Almanac) -> int:
    seed_ranges = batched(almanac.seeds, 2)
    min_candidates = []
    for seed_range in seed_ranges:
        transformed_ranges = almanac.apply_all_transformations_range(seed_range)
        min_candidates.append(min(map(lambda r: r[0], transformed_ranges)))
    return min(min_candidates)
