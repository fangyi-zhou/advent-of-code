from typing import List, Tuple, Set


def parse(lines: str) -> List[str]:
    return lines.split("\n")


def part1(maze: List[str]) -> int:
    start_pos = (0, -1)
    start_dir = (0, 1)
    visited: Set[Tuple[int, int]] = set()
    processed: Set[Tuple[Tuple[int, int], Tuple[int, int]]] = set()
    worklist = [(start_pos, start_dir)]
    while worklist:
        pos, direction = worklist.pop()
        x, y = pos
        dx, dy = direction
        curr_x, curr_y = x + dx, y + dy
        if (
            (pos, direction) in processed
            or curr_x < 0
            or curr_x >= len(maze)
            or curr_y < 0
            or curr_y >= len(maze[0])
        ):
            continue
        visited.add((curr_x, curr_y))
        processed.add((pos, direction))
        if maze[curr_x][curr_y] == ".":
            worklist.append(((curr_x, curr_y), direction))
        elif maze[curr_x][curr_y] == "/":
            worklist.append(((curr_x, curr_y), (-dy, -dx)))
        elif maze[curr_x][curr_y] == "\\":
            worklist.append(((curr_x, curr_y), (dy, dx)))
        elif maze[curr_x][curr_y] == "|":
            if dx:
                worklist.append(((curr_x, curr_y), direction))
            else:
                worklist.append(((curr_x, curr_y), (1, 0)))
                worklist.append(((curr_x, curr_y), (-1, 0)))
        elif maze[curr_x][curr_y] == "-":
            if dy:
                worklist.append(((curr_x, curr_y), direction))
            else:
                worklist.append(((curr_x, curr_y), (0, 1)))
                worklist.append(((curr_x, curr_y), (0, -1)))
    # for i in range(len(maze)):
    #     for j in range(len(maze[0])):
    #         print("#" if (i, j) in visited else maze[i][j], end="")
    #     print()

    return len(visited)
