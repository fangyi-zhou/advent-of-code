from dataclasses import dataclass
from typing import List


@dataclass
class Card:
    card_id: int
    my_numbers: List[int]
    winning_numbers: List[int]


def parse(lines: str) -> List[Card]:
    def parse_card(line: str) -> Card:
        card_id, numbers = line.split(":")
        card_id = card_id.split()[-1]
        parsed_card_id = int(card_id)
        my_nums_raw, winning_nums_raw = numbers.split("|")
        my_numbers = [*map(int, my_nums_raw.strip().split())]
        winning_numbers = [*map(int, winning_nums_raw.strip().split())]
        return Card(
            card_id=parsed_card_id,
            my_numbers=my_numbers,
            winning_numbers=winning_numbers,
        )

    return [*map(parse_card, lines.split("\n"))]


def part1(cards: List[Card]) -> int:
    def score_points(card: Card) -> int:
        my_numbers = set(card.my_numbers)
        winning_numbers = set(card.winning_numbers)
        wins = len(my_numbers & winning_numbers)
        return pow(2, wins - 1) if wins else 0

    return sum(map(score_points, cards))
