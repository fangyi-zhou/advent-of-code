from typing import List


TData = List[str]
TPart1 = int


def parse(input_data: str) -> TData:
    """Parse input into list of battery-bank strings (digits only).

    Empty lines are ignored and whitespace is stripped.
    """
    return [line.strip() for line in input_data.splitlines() if line.strip()]


def max_subsequence_digits(bank: str, k: int) -> str:
    """Return the lexicographically-maximal subsequence of length `k` from `bank`.

    Implements a linear-time monotonic-stack algorithm. If `k` >= len(bank),
    returns the whole bank string.
    """
    n = len(bank)
    if k >= n:
        return bank
    drop = n - k
    stack: List[str] = []
    for ch in bank:
        while stack and drop > 0 and stack[-1] < ch:
            stack.pop()
            drop -= 1
        stack.append(ch)
    return "".join(stack[:k])


def max_joltage_for_bank_k(bank: str, k: int) -> int:
    """Return the maximum k-digit joltage obtainable from `bank`.

    Uses `max_subsequence_digits` then converts to int.
    """
    subseq = max_subsequence_digits(bank, k)
    return int(subseq)


def max_joltage_for_bank(bank: str) -> int:
    """Compatibility wrapper for the original 2-digit problem."""
    return max_joltage_for_bank_k(bank, 2)


def part1(data: TData) -> TPart1:
    """Compute the total output joltage for k=2: sum of per-bank maximums."""
    return sum(max_joltage_for_bank_k(bank, 2) for bank in data)


def part2(data: TData, k: int = 12) -> int:
    """Compute the total output joltage for selecting `k` digits per bank."""
    return sum(max_joltage_for_bank_k(bank, k) for bank in data)


if __name__ == "__main__":
    import sys

    args = sys.argv[1:]
    path = None
    k: int | None = None

    # Determine path (first arg that does not start with '-') if present
    if args and not args[0].startswith("-"):
        path = args[0]

    # Determine k: numeric arg or `--part2` flag
    for a in args:
        if a == "--part2" or a == "part2":
            k = 12
        else:
            try:
                kv = int(a)
            except Exception:
                kv = None
            if kv is not None and kv > 0:
                k = kv

    if path:
        with open(path, "r") as f:
            inp = f.read()
    else:
        inp = sys.stdin.read()

    data = parse(inp)

    if k is None:
        # default: part1 behavior (k=2)
        print(part1(data))
    else:
        print(part2(data, k))
