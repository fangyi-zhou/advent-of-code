from typing import List, Tuple

TData = Tuple[List[Tuple[int, int]], List[int]]
TPart1 = int


def parse(input_data: str) -> TData:
	"""Parse input into a list of ranges and a list of available IDs.

	Ranges are lines like `3-5`. After a blank line, each following line is a
	single available ingredient ID.
	"""
	parts = input_data.strip().splitlines()
	ranges: List[Tuple[int, int]] = []
	ids: List[int] = []
	mode = 'ranges'
	for line in parts:
		line = line.strip()
		if line == "":
			mode = 'ids'
			continue
		if mode == 'ranges':
			if '-' in line:
				a, b = line.split('-', 1)
				ranges.append((int(a), int(b)))
		else:
			ids.append(int(line))
	return ranges, ids


def _merge_ranges(ranges: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
	if not ranges:
		return []
	ranges_sorted = sorted(ranges, key=lambda x: x[0])
	merged: List[Tuple[int, int]] = []
	cur_lo, cur_hi = ranges_sorted[0]
	for lo, hi in ranges_sorted[1:]:
		if lo <= cur_hi + 1:
			cur_hi = max(cur_hi, hi)
		else:
			merged.append((cur_lo, cur_hi))
			cur_lo, cur_hi = lo, hi
	merged.append((cur_lo, cur_hi))
	return merged


def part1(data: TData) -> TPart1:
	"""Count how many available IDs fall inside any fresh range."""
	ranges, ids = data
	if not ranges or not ids:
		return 0
	merged = _merge_ranges(ranges)
	count = 0
	for x in ids:
		# binary/linear search through merged intervals
		found = False
		for lo, hi in merged:
			if lo <= x <= hi:
				found = True
				break
			if x < lo:
				# intervals sorted; if x is before current interval, it's not in any
				break
		if found:
			count += 1
	return count


def part2(data: TData) -> int:
	"""Return the total number of unique ingredient IDs considered fresh by
	the ranges (i.e., the size of the union of all ranges)."""
	ranges, _ = data
	if not ranges:
		return 0
	merged = _merge_ranges(ranges)
	total = sum(hi - lo + 1 for lo, hi in merged)
	return total


if __name__ == '__main__':
	import sys

	args = sys.argv[1:]
	path = None
	for a in args:
		# treat as path
		path = a

	if path:
		with open(path, 'r') as f:
			data = parse(f.read())
	else:
		data = parse(sys.stdin.read())

	print(part1(data))
