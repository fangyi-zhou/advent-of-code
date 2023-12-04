from dataclasses import dataclass
from functools import reduce
from typing import Dict, List, Tuple


@dataclass
class Game:
    game_id: int
    reveals: List[Dict[str, int]]


def parse(lines: str) -> List[Game]:
    def parse_cube(line: str) -> Tuple[str, int]:
        count, colour = line.strip().split(" ")
        return (colour, int(count))

    def parse_reveal(line: str) -> Dict[str, int]:
        cubes = line.strip().split(",")
        return dict(map(parse_cube, cubes))

    def parse_line(line: str) -> Game:
        game_id, reveal_raw = line.split(":")
        parsed_game_id = int(game_id.split(" ")[1])
        reveals = reveal_raw.strip().split(";")
        parsed_reveals = [*map(parse_reveal, reveals)]
        return Game(game_id=parsed_game_id, reveals=parsed_reveals)

    return [*map(parse_line, lines.split("\n"))]


def part1(games: List[Game]) -> int:
    def possible_game(game: Game) -> bool:
        return all(
            (
                reveal.get("red", 0) <= 12
                and reveal.get("green", 0) <= 13
                and reveal.get("blue", 0) <= 14
            )
            for reveal in game.reveals
        )

    return sum(map(lambda game: game.game_id, filter(possible_game, games)))


def part2(games: List[Game]) -> int:
    def min_cubes(game: Game) -> int:
        cubes_required = reduce(
            lambda rev1, rev2: {
                "red": max(rev1.get("red", 0), rev2.get("red", 0)),
                "green": max(rev1.get("green", 0), rev2.get("green", 0)),
                "blue": max(rev1.get("blue", 0), rev2.get("blue", 0)),
            },
            game.reveals,
            {"red": 0, "green": 0, "blue": 0},
        )
        return cubes_required["red"] * cubes_required["green"] * cubes_required["blue"]

    return sum(map(min_cubes, games))
