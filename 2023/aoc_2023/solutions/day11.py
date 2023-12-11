from typing import List, Tuple
from itertools import combinations
from dataclasses import dataclass


@dataclass
class GalaxyMap:
    empty_rows: List[int]
    empty_cols: List[int]
    galaxies: List[Tuple[int, int]]


def parse(lines_raw: str) -> GalaxyMap:
    lines = lines_raw.split("\n")
    empty_rows = [
        i
        for i in range(len(lines))
        if all(lines[i][j] == "." for j in range(len(lines[0])))
    ]
    empty_cols = [
        j
        for j in range(len(lines[0]))
        if all(lines[i][j] == "." for i in range(len(lines)))
    ]
    galaxies = [
        (i, j)
        for i in range(len(lines))
        for j in range(len(lines[0]))
        if lines[i][j] == "#"
    ]
    return GalaxyMap(empty_rows=empty_rows, empty_cols=empty_cols, galaxies=galaxies)


def part1(galaxy_map: GalaxyMap) -> int:
    dists = 0
    for g1, g2 in combinations(galaxy_map.galaxies, 2):
        x1, y1 = g1
        x2, y2 = g2
        dist = (
            abs(x1 - x2)
            + abs(y1 - y2)
            + len(
                [
                    x
                    for x in range(min(x1, x2), max(x1, x2))
                    if x in galaxy_map.empty_rows
                ]
            )
            + len(
                [
                    y
                    for y in range(min(y1, y2), max(y1, y2))
                    if y in galaxy_map.empty_cols
                ]
            )
        )
        dists += dist
    return dists


def part2(galaxy_map: GalaxyMap) -> int:
    dists = 0
    for g1, g2 in combinations(galaxy_map.galaxies, 2):
        x1, y1 = g1
        x2, y2 = g2
        dist = (
            abs(x1 - x2)
            + abs(y1 - y2)
            + 999999
            * len(
                [
                    x
                    for x in range(min(x1, x2), max(x1, x2))
                    if x in galaxy_map.empty_rows
                ]
            )
            + 999999
            * len(
                [
                    y
                    for y in range(min(y1, y2), max(y1, y2))
                    if y in galaxy_map.empty_cols
                ]
            )
        )
        dists += dist
    return dists
