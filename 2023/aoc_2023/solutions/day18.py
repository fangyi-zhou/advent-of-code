from typing import List, Tuple, Set


def parse(lines: str) -> List[Tuple[str, int, str]]:
    def parse_line(line: str) -> Tuple[str, int, str]:
        direction, num_str, colour = line.split()
        num = int(num_str)
        colour = colour[1:-1]
        return direction, num, colour

    return [*map(parse_line, lines.split("\n"))]


DIRECTION = {
    "R": (1, 0),
    "L": (-1, 0),
    "U": (0, -1),
    "D": (0, 1),
}


def part1(plan: List[Tuple[str, int, str]]) -> int:
    curr_x, curr_y = 0, 0
    min_x, max_x = 0, 0
    min_y, max_y = 0, 0
    walls: Set[Tuple[int, int]] = set([(curr_x, curr_y)])
    for line in plan:
        direction, num, _ = line
        dx, dy = DIRECTION[direction]
        for i in range(num):
            curr_x, curr_y = curr_x + dx, curr_y + dy
            min_x = min(min_x, curr_x)
            min_y = min(min_y, curr_y)
            max_x = max(max_x, curr_x)
            max_y = max(max_y, curr_y)
            walls.add((curr_x, curr_y))
    exterior: Set[Tuple[int, int]] = set()

    def fill(i: int, j: int) -> None:
        task_list: List[Tuple[int, int]] = []
        task_list.append((i, j))
        while task_list:
            i, j = task_list.pop()
            if (i, j) in walls or (i, j) in exterior:
                continue
            exterior.add((i, j))
            if i > min_x:
                task_list.append((i - 1, j))
            if i < max_x:
                task_list.append((i + 1, j))
            if j > min_y:
                task_list.append((i, j - 1))
            if j < max_y:
                task_list.append((i, j + 1))

    for i in range(min_x, max_x + 1):
        fill(i, min_y)
        fill(i, max_y)
    for j in range(min_y, max_y + 1):
        fill(min_x, j)
        fill(max_x, j)
    # for y in range(min_y, max_y + 1):
    #     for x in range(min_x, max_x + 1):
    #         print(
    #             "#" if (x, y) in walls else ("E" if (x, y) in exterior else "."), end=""
    #         )
    #     print()
    return (max_x - min_x + 1) * (max_y - min_y + 1) - len(exterior)
