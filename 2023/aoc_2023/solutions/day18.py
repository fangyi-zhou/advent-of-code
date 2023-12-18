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
    area = 0
    walls: Set[Tuple[int, int]] = set([(curr_x, curr_y)])
    for line in plan:
        direction, num, _ = line
        dx, dy = DIRECTION[direction]
        if direction == "U":
            base = -curr_x
        elif direction == "D":
            base = curr_x
        elif direction == "L":
            base = curr_y
        else:
            base = -curr_y
        area += num * (base + 1)
        for i in range(num):
            curr_x, curr_y = curr_x + dx, curr_y + dy
            min_x = min(min_x, curr_x)
            min_y = min(min_y, curr_y)
            max_x = max(max_x, curr_x)
            max_y = max(max_y, curr_y)
            walls.add((curr_x, curr_y))
    # for y in range(min_y, max_y + 1):
    #     for x in range(min_x, max_x + 1):
    #         if x == 0 and y == 0:
    #             print("S", end="")
    #         else:
    #             print("#" if (x, y) in walls else ".", end="")
    #     print()
    return area // 2 + 1
