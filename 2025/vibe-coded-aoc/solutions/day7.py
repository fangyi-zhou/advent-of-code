def parse(input_data):
    lines = input_data.strip().split('\n')
    return lines


def part1(manifold):
    """
    Simulate tachyon beam from S downward.
    When beam hits splitter (^), it splits into left and right beams.
    Both new beams continue downward.
    Count total number of splits.
    """
    # Find the starting position (S)
    start_col = -1
    start_row = -1

    for row, line in enumerate(manifold):
        for col, char in enumerate(line):
            if char == 'S':
                start_col = col
                start_row = row
                break
        if start_col != -1:
            break

    if start_col == -1:
        return 0

    # Simulate beam propagation
    # Track which (row, col) pairs we've already split at to avoid recounting
    split_positions = set()

    # Active beams: column positions at current row that are moving downward
    beams = [start_col]
    split_count = 0
    current_row = start_row

    while beams and current_row < len(manifold) - 1:
        current_row += 1
        new_beams = []

        for col in beams:
            # Check bounds
            if col < 0 or col >= len(manifold[current_row]):
                continue

            cell = manifold[current_row][col]

            if cell == '^':
                # Hit a splitter - count this as a split
                split_pos = (current_row, col)
                if split_pos not in split_positions:
                    split_count += 1
                    split_positions.add(split_pos)

                # Create two new beams at left and right of splitter
                # These continue downward from next row
                left_col = col - 1
                right_col = col + 1

                if left_col >= 0 and left_col < len(manifold[current_row]):
                    new_beams.append(left_col)
                if right_col >= 0 and right_col < len(manifold[current_row]):
                    new_beams.append(right_col)
            else:
                # Empty space (.) or other - continue downward
                new_beams.append(col)

        # Remove duplicate beam positions (multiple beams in same column)
        beams = list(set(new_beams))

    return split_count
def part2(manifold):
    """
    Count the number of unique timelines.
    Each timeline is a distinct path the particle takes.
    At each splitter, the particle branches into 2 new timelines.
    Count the total number of leaf paths (distinct ways to reach the bottom).
    """
    # Find the starting position (S)
    start_col = -1
    start_row = -1

    for row, line in enumerate(manifold):
        for col, char in enumerate(line):
            if char == 'S':
                start_col = col
                start_row = row
                break
        if start_col != -1:
            break

    if start_col == -1:
        return 0

    # Memoization: memo[(row, col)] = number of timelines from this position onward
    memo = {}

    def count_timelines_from(row, col):
        """Count the number of distinct timelines starting from (row, col) moving downward."""
        if (row, col) in memo:
            return memo[(row, col)]

        current_row = row
        current_col = col

        # Move downward until we hit a splitter or reach the end
        while current_row < len(manifold) - 1:
            current_row += 1

            # Check bounds
            if current_col < 0 or current_col >= len(manifold[current_row]):
                # Out of bounds - this is one timeline
                result = 1
                memo[(row, col)] = result
                return result

            cell = manifold[current_row][current_col]

            if cell == '^':
                # Hit a splitter - count timelines from left and right
                left_col = current_col - 1
                right_col = current_col + 1

                left_timelines = 0
                right_timelines = 0

                if left_col >= 0 and left_col < len(manifold[current_row]):
                    left_timelines = count_timelines_from(current_row, left_col)
                else:
                    left_timelines = 1  # Out of bounds path counts as 1 timeline

                if right_col >= 0 and right_col < len(manifold[current_row]):
                    right_timelines = count_timelines_from(current_row, right_col)
                else:
                    right_timelines = 1  # Out of bounds path counts as 1 timeline

                result = left_timelines + right_timelines
                memo[(row, col)] = result
                return result
            # else: empty space, continue downward

        # Reached the bottom
        result = 1
        memo[(row, col)] = result
        return result

    return count_timelines_from(start_row, start_col)
