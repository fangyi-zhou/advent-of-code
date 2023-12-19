from typing import List


def parse(lines: str) -> List[str]:
    return lines.split("\n")


def part1(lines: List[str]) -> int:
    score = 0
    for j in range(len(lines[0])):
        last_wall = -1
        rocks = 0
        for i in range(len(lines)):
            if lines[i][j] == "O":
                rocks += 1
            elif lines[i][j] == "#":
                if rocks:
                    points = (len(lines) * 2 - last_wall * 2 - rocks - 1) * rocks // 2
                    score += points
                last_wall = i
                rocks = 0
        if rocks:
            points = (len(lines) * 2 - last_wall * 2 - rocks - 1) * rocks // 2
            score += points
    return score
