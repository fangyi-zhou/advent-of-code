import heapq
from typing import List, Tuple, Set


Point = Tuple[int, int]


def parse(maze: str) -> List[List[int]]:
    return [[int(ch) for ch in line] for line in maze.split("\n")]


DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]


def _print_path(maze: List[List[int]], path: List[Tuple[int, int]]) -> None:
    for i in range(len(maze)):
        for j in range(len(maze[0])):
            print("#" if (i, j) in path else maze[i][j], end="")
        print()


def part1(maze: List[List[int]]) -> int:
    start = (0, 0)
    worklist: List[Tuple[int, Point, Tuple[Point, int], List[Point]]] = []
    visited: Set[Tuple[Point, Tuple[Point, int]]] = set()
    heapq.heappush(worklist, (maze[0][1], (0, 1), ((0, 1), 1), [(0, 0)]))
    heapq.heappush(worklist, (maze[1][0], (1, 0), ((1, 0), 1), [(0, 0)]))
    while worklist:
        existing_distance, curr, history, path = heapq.heappop(worklist)
        if curr in path:
            continue
        if (curr, history) in visited:
            continue
        visited.add((curr, history))
        new_path = path.copy()
        new_path.append(curr)
        if curr == (len(maze) - 1, len(maze[0]) - 1):
            # _print_path(maze, path)
            return existing_distance
        visited.add((curr, history))
        for curr_dir in DIRECTIONS:
            dx, dy = curr_dir
            x, y = curr
            next_x, next_y = x + dx, y + dy
            last_dir, last_dir_count = history
            if (
                next_x < 0
                or next_x >= len(maze)
                or next_y < 0
                or next_y >= len(maze[0])
            ):
                continue
            new_dist = existing_distance + maze[next_x][next_y]
            if (dx, dy) == last_dir:
                if last_dir_count < 3:
                    heapq.heappush(
                        worklist,
                        (
                            new_dist,
                            (next_x, next_y),
                            (last_dir, last_dir_count + 1),
                            new_path,
                        ),
                    )
            else:
                heapq.heappush(
                    worklist, (new_dist, (next_x, next_y), (curr_dir, 1), new_path)
                )
        # print(existing_distance, curr, history)
    raise RuntimeError("Unreachable")


def part2(maze: List[List[int]]) -> int:
    start = (0, 0)
    worklist: List[Tuple[int, Point, Tuple[Point, int], List[Point]]] = []
    visited: Set[Tuple[Point, Tuple[Point, int]]] = set()
    heapq.heappush(worklist, (maze[0][1], (0, 1), ((0, 1), 1), [(0, 0)]))
    heapq.heappush(worklist, (maze[1][0], (1, 0), ((1, 0), 1), [(0, 0)]))
    while worklist:
        existing_distance, curr, history, path = heapq.heappop(worklist)
        if curr in path:
            continue
        if (curr, history) in visited:
            continue
        visited.add((curr, history))
        new_path = path.copy()
        new_path.append(curr)
        last_dir, last_dir_count = history
        if curr == (len(maze) - 1, len(maze[0]) - 1) and last_dir_count >= 4:
            # _print_path(maze, path)
            return existing_distance
        visited.add((curr, history))
        if last_dir_count < 4:
            directions = [last_dir]
        else:
            directions = DIRECTIONS
        for curr_dir in directions:
            dx, dy = curr_dir
            x, y = curr
            next_x, next_y = x + dx, y + dy
            if (
                next_x < 0
                or next_x >= len(maze)
                or next_y < 0
                or next_y >= len(maze[0])
            ):
                continue
            new_dist = existing_distance + maze[next_x][next_y]
            if (dx, dy) == last_dir:
                if last_dir_count < 10:
                    heapq.heappush(
                        worklist,
                        (
                            new_dist,
                            (next_x, next_y),
                            (last_dir, last_dir_count + 1),
                            new_path,
                        ),
                    )
            else:
                heapq.heappush(
                    worklist, (new_dist, (next_x, next_y), (curr_dir, 1), new_path)
                )
        # print(existing_distance, curr, history)
    raise RuntimeError("Unreachable")
