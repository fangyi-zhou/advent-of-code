from typing import List
import re


def parse(lines: str) -> List[str]:
    return lines.split("\n")


def part1(lines: List[str]) -> int:
    def search_for_parts(x: int, y_start: int, length: int) -> bool:
        for i in range(x - 1, x + 2):
            for j in range(y_start - 1, y_start + length + 1):
                if i >= 0 and i < len(lines) and j >= 0 and j < len(lines[0]):
                    if lines[i][j] not in ".0123456789":
                        return False
        return True

    results: List[int] = []
    for i, line in enumerate(lines):
        digit_buf = ""
        for j, ch in enumerate(line):
            if ch >= "0" and ch <= "9":
                digit_buf += ch
            else:
                if digit_buf:
                    if not search_for_parts(i, j - len(digit_buf), len(digit_buf)):
                        results.append(int(digit_buf))
                    digit_buf = ""
        j += 1
        if digit_buf:
            if not search_for_parts(i, j - len(digit_buf), len(digit_buf)):
                results.append(int(digit_buf))
    return sum(results)
