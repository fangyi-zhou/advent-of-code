from typing import Tuple, List, Optional, Set


def parse(lines: str) -> Tuple[List[str], Tuple[int, int]]:
    maze = lines.split("\n")
    for i, line in enumerate(maze):
        for j, tile in enumerate(line):
            if maze[i][j] == "S":
                return maze, (i, j)
    raise RuntimeError("Unreachable")


def _follow_pipe(
    tile: str, prev: Tuple[int, int], curr: Tuple[int, int]
) -> Optional[Tuple[int, int]]:
    prev_x, prev_y = prev
    curr_x, curr_y = curr
    if tile == "|":
        if prev_y == curr_y:
            dx = curr_x - prev_x
            return curr_x + dx, curr_y
    elif tile == "-":
        if prev_x == curr_x:
            dy = curr_y - prev_y
            return curr_x, curr_y + dy
    elif tile == "L":
        if prev_x + 1 == curr_x and prev_y == curr_y:
            return curr_x, curr_y + 1
        if prev_x == curr_x and prev_y == curr_y + 1:
            return curr_x - 1, curr_y
    elif tile == "J":
        if prev_x + 1 == curr_x and prev_y == curr_y:
            return curr_x, curr_y - 1
        if prev_x == curr_x and prev_y + 1 == curr_y:
            return curr_x - 1, curr_y
    elif tile == "7":
        if prev_x == curr_x + 1 and prev_y == curr_y:
            return curr_x, curr_y - 1
        if prev_x == curr_x and prev_y + 1 == curr_y:
            return curr_x + 1, curr_y
    elif tile == "F":
        if prev_x == curr_x + 1 and prev_y == curr_y:
            return curr_x, curr_y + 1
        if prev_x == curr_x and prev_y == curr_y + 1:
            return curr_x + 1, curr_y
    return None


DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]


def part1(inputs: Tuple[List[str], Tuple[int, int]]) -> int:
    maze, start = inputs
    start_x, start_y = start
    for direction in DIRECTIONS:
        dx, dy = direction
        path: Set[Tuple[int, int]] = set()
        path.add(start)
        prev = start
        curr = start_x + dx, start_y + dy
        while curr:
            x, y = curr
            next_coord = _follow_pipe(maze[x][y], prev, curr)
            if not next_coord:
                break
            path.add(curr)
            prev = curr
            curr = next_coord
        if curr == start:
            return len(path) // 2
    raise RuntimeError("Unreachable")
