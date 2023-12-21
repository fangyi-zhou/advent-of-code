from typing import Tuple, List


def parse(lines: str) -> Tuple[List[str], Tuple[int, int]]:
    maze = lines.split("\n")
    for i, line in enumerate(maze):
        for j, tile in enumerate(line):
            if maze[i][j] == "S":
                return maze, (i, j)
    raise RuntimeError("Unreachable")
    return lines.split("\n")


DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]


def part1(inputs: Tuple[List[str], Tuple[int, int]]) -> int:
    maze, start = inputs
    current = set([start])
    for _ in range(64):
        next_reachable = set()
        for x, y in current:
            for dx, dy in DIRECTIONS:
                next_x, next_y = x + dx, y + dy
                if (
                    next_x >= 0
                    and next_x < len(maze)
                    and next_y >= 0
                    and next_y < len(maze[0])
                    and maze[next_x][next_y] != "#"
                ):
                    next_reachable.add((next_x, next_y))
        current = next_reachable
    return len(current)
