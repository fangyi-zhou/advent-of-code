from typing import List, Tuple

TData = List[Tuple[str, int]]
TPart1 = int


def parse(input_data: str) -> TData:
    """Parse input into a list of (direction, distance) tuples.

    Each non-empty line is expected to be like 'L68' or 'R48'.
    """
    lines = [line.strip() for line in input_data.splitlines() if line.strip()]
    result: TData = []
    for line in lines:
        dir_char = line[0]
        dist = int(line[1:])
        result.append((dir_char, dist))
    return result


def part1(data: TData) -> TPart1:
    """Simulate the dial and count how many times it lands on 0.

    The dial has numbers 0..99 and starts at 50. 'L' moves toward lower
    numbers (decreasing), 'R' moves toward higher numbers (increasing).
    """
    pos = 50
    count = 0
    for direction, dist in data:
        if direction == "L":
            pos = (pos - dist) % 100
        else:
            pos = (pos + dist) % 100
        if pos == 0:
            count += 1
    return count
