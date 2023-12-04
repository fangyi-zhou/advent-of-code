from dataclasses import dataclass
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
