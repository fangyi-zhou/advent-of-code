from typing import List, Tuple

TData = List[Tuple[str, int]]
TPart1 = int
TPart2 = int


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

def part2(data: TData) -> TPart2:
    """Count every time the dial points at 0 during any click.

    For each rotation of distance `d`, we count k in 1..d such that
    (pos + s*k) % 100 == 0 where s is +1 for 'R' and -1 for 'L'.
    This can be computed arithmetically instead of iterating each click.
    """
    pos = 50
    total = 0
    for direction, dist in data:
        s = 1 if direction == "R" else -1
        # solve s*k ≡ -pos (mod 100) => k ≡ -s*pos (mod 100)
        target = (-s * pos) % 100
        # smallest positive k that satisfies congruence
        first_k = target if target != 0 else 100
        if first_k <= dist:
            # number of solutions within 1..dist stepping by 100
            total += 1 + (dist - first_k) // 100
        # update position after the full rotation
        pos = (pos + s * dist) % 100
    return total
