from dataclasses import dataclass
from typing import List, Set


@dataclass
class Card:
    card_id: int
    my_numbers: Set[int]
    winning_numbers: Set[int]

    def matches(self) -> int:
        return len(self.my_numbers & self.winning_numbers)


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
            my_numbers=set(my_numbers),
            winning_numbers=set(winning_numbers),
        )

    return [*map(parse_card, lines.split("\n"))]


def part1(cards: List[Card]) -> int:
    def score_points(card: Card) -> int:
        wins = card.matches()
        return pow(2, wins - 1) if wins else 0

    return sum(map(score_points, cards))


def part2(cards: List[Card]) -> int:
    card_counts = [1] * len(cards)
    for i, card in enumerate(cards):
        wins = card.matches()
        for j in range(wins):
            if i + j + 1 < len(cards):
                card_counts[i + j + 1] += card_counts[i]
    return sum(card_counts)
