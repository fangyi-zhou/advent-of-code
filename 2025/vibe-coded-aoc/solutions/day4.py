from typing import List, Tuple

TData = List[str]
TPart1 = int


def parse(input_data: str) -> TData:
    """Parse the input into a list of strings representing the grid."""
    return [line.rstrip() for line in input_data.strip().splitlines()]


def _neighbors(r: int, c: int, rows: int, cols: int) -> List[Tuple[int, int]]:
    deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    res = []
    for dr, dc in deltas:
        nr, nc = r + dr, c + dc
        if 0 <= nr < rows and 0 <= nc < cols:
            res.append((nr, nc))
    return res


def part1(data: TData) -> TPart1:
    """Count the number of rolls ('@') that have fewer than 4 adjacent rolls.

    A roll is accessible by a forklift if among its eight adjacent cells there
    are fewer than four '@' characters.
    """
    if not data:
        return 0
    rows = len(data)
    cols = len(data[0])
    grid = data
    count = 0
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] != '@':
                continue
            adj = 0
            for nr, nc in _neighbors(r, c, rows, cols):
                if grid[nr][nc] == '@':
                    adj += 1
                    if adj >= 4:
                        break
            if adj < 4:
                count += 1
    return count


def part2(data: TData) -> int:
    """Simulate removing accessible rolls repeatedly and return total removed."""
    if not data:
        return 0
    rows = len(data)
    cols = len(data[0])
    grid = [list(row) for row in data]
    removed = 0
    while True:
        to_remove: List[Tuple[int, int]] = []
        for r in range(rows):
            for c in range(cols):
                if grid[r][c] != '@':
                    continue
                adj = 0
                for nr, nc in _neighbors(r, c, rows, cols):
                    if grid[nr][nc] == '@':
                        adj += 1
                        if adj >= 4:
                            break
                if adj < 4:
                    to_remove.append((r, c))
        if not to_remove:
            break
        for r, c in to_remove:
            grid[r][c] = '.'
            removed += 1
    return removed


if __name__ == '__main__':
    import sys

    # Determine input source (first argument if present)
    args = sys.argv[1:]
    path = None
    part = 1
    for a in args:
        if a in ('2', '--part2', 'part2'):
            part = 2
        elif a in ('1', '--part1', 'part1'):
            part = 1
        else:
            # treat as path
            path = a

    if path:
        with open(path, 'r') as f:
            data = parse(f.read())
    else:
        data = parse(sys.stdin.read())

    if part == 1:
        print(part1(data))
    else:
        print(part2(data))
