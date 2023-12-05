from dataclasses import dataclass
from typing import List


@dataclass
class RangeEntry:
    destination: int
    source: int
    length: int


@dataclass
class Transformation:
    name: str
    entries: List[RangeEntry]

    def apply(self, src: int) -> int:
        for entry in self.entries:
            if src >= entry.source and src < entry.source + entry.length:
                return entry.destination + src - entry.source
        return src


@dataclass
class Almanac:
    seeds: List[int]
    maps: List[Transformation]

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
    transformations: List[Transformation] = []
    for line in lines[2:]:
        if "map" in line:
            name = line.split()[0]
            continue
        if not line:
            transformations.append(
                Transformation(name=name, entries=range_entries_buffer)
            )
            range_entries_buffer = []
            continue
        destination, source, length = [*map(int, line.split())]
        range_entries_buffer.append(
            RangeEntry(destination=destination, source=source, length=length)
        )
    transformations.append(Transformation(name=name, entries=range_entries_buffer))
    return Almanac(seeds=seeds, maps=transformations)


def part1(almanac: Almanac) -> int:
    return min(map(lambda src: almanac.apply_all_transformations(src), almanac.seeds))
