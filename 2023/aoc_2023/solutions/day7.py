from collections import defaultdict
from typing import Dict, List, Tuple


def parse(lines: str) -> List[Tuple[str, int]]:
    def parse_line(line: str) -> Tuple[str, int]:
        hand, bid_str = line.split()
        return hand, int(bid_str)

    return [*map(parse_line, lines.split("\n"))]


def part1(plays: List[Tuple[str, int]]) -> int:
    card_ord = {
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

    def summarise_hand(hand: str) -> List[int]:
        histogram: Dict[str, int] = defaultdict(int)
        for card in hand:
            histogram[card] += 1
        return [*reversed(sorted(histogram.values()))]

    def hand_sort_key(hand: str) -> Tuple[List[int], List[int]]:
        return summarise_hand(hand), [card_ord[card] for card in hand]

    plays = sorted(plays, key=lambda play: hand_sort_key(play[0]))
    result = 0
    for i, (play, bid) in enumerate(plays):
        result += (i + 1) * bid
    return result


def part2(plays: List[Tuple[str, int]]) -> int:
    card_ord = {
        "J": 1,
        "2": 2,
        "3": 3,
        "4": 4,
        "5": 5,
        "6": 6,
        "7": 7,
        "8": 8,
        "9": 9,
        "T": 10,
        "Q": 11,
        "K": 12,
        "A": 13,
    }

    def summarise_hand(hand: str) -> List[int]:
        histogram: Dict[str, int] = defaultdict(int)
        for card in hand:
            histogram[card] += 1
        jokers = histogram.get("J", 0)
        if "J" in histogram:
            del histogram["J"]
        summary = [*reversed(sorted(histogram.values()))]
        if summary:
            summary[0] += jokers
            return summary
        else:
            return [jokers]

    def hand_sort_key(hand: str) -> Tuple[List[int], List[int]]:
        return summarise_hand(hand), [card_ord[card] for card in hand]

    plays = sorted(plays, key=lambda play: hand_sort_key(play[0]))
    result = 0
    for i, (play, bid) in enumerate(plays):
        result += (i + 1) * bid
    return result
