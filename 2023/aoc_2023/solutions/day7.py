from collections import defaultdict
from typing import Dict, List, Tuple


def parse(lines: str) -> List[Tuple[str, int]]:
    def parse_line(line: str) -> Tuple[str, int]:
        hand, bid_str = line.split()
        return hand, int(bid_str)

    return [*map(parse_line, lines.split("\n"))]


CARD_ORD = {
    "2": 1,
    "3": 2,
    "4": 3,
    "5": 4,
    "6": 5,
    "7": 6,
    "8": 7,
    "9": 8,
    "T": 9,
    "J": 10,
    "Q": 11,
    "K": 12,
    "A": 13,
}


def part1(plays: List[Tuple[str, int]]) -> int:
    def summarise_hand(hand: str) -> List[int]:
        histogram: Dict[str, int] = defaultdict(int)
        for card in hand:
            histogram[card] += 1
        return [*reversed(sorted(histogram.values()))]

    def hand_sort_key(hand: str) -> Tuple[List[int], List[int]]:
        return summarise_hand(hand), [CARD_ORD[card] for card in hand]

    plays = sorted(plays, key=lambda play: hand_sort_key(play[0]))
    result = 0
    for i, (play, bid) in enumerate(plays):
        result += (i + 1) * bid
    return result
