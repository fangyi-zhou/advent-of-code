import re


def parse(input_data):
    lines = input_data.strip().split('\n')

    # Last line contains operations
    op_line = lines[-1]
    num_lines = lines[:-1]

    # Pad all lines to the same length
    max_len = max(len(line) for line in lines)
    num_lines = [line.ljust(max_len) for line in num_lines]
    op_line = op_line.ljust(max_len)

    return {
        'num_lines': num_lines,
        'op_line': op_line,
        'max_len': max_len
    }


def solve_worksheet(data):
    num_lines = data['num_lines']
    op_line = data['op_line']
    max_len = data['max_len']

    # Find all operation positions and their groups of numbers
    total = 0
    processed = set()

    for op_pos, op_char in enumerate(op_line):
        if op_char not in ['+', '*']:
            continue
        if op_pos in processed:
            continue

        # Find the range of this problem
        # Look left and right to find where digits exist
        group_start = op_pos
        group_end = op_pos

        # Expand left
        while group_start > 0:
            # Check if there's a digit anywhere in this column
            has_digit = any(num_lines[row][group_start - 1].isdigit() for row in range(len(num_lines)))
            if has_digit:
                group_start -= 1
            else:
                break

        # Expand right
        while group_end < max_len - 1:
            # Check if there's a digit anywhere in this column
            has_digit = any(num_lines[row][group_end + 1].isdigit() for row in range(len(num_lines)))
            if has_digit:
                group_end += 1
            else:
                break

        # Extract all numbers in this range
        numbers = []
        for row in num_lines:
            text = row[group_start:group_end + 1]
            # Find all numbers in this text
            nums = re.findall(r'\d+', text)
            for num_str in nums:
                numbers.append(int(num_str))

        # Calculate result
        if numbers:
            result = numbers[0]
            for num in numbers[1:]:
                if op_char == '+':
                    result += num
                elif op_char == '*':
                    result *= num
            total += result

            # Mark this range as processed
            for i in range(group_start, group_end + 1):
                processed.add(i)

    return total


def part1(data):
    return solve_worksheet(data)


def part2(data):
    num_lines = data['num_lines']
    op_line = data['op_line']
    max_len = data['max_len']

    # Read right-to-left, one column at a time
    total = 0
    processed = set()

    # Start from the rightmost position and go backwards
    col = max_len - 1
    while col >= 0:
        # Skip empty columns (going backwards)
        while col >= 0 and op_line[col] == ' ' and all(num_lines[row][col] == ' ' for row in range(len(num_lines))):
            col -= 1

        if col < 0:
            break

        # Found a problem group, now find its extent going leftwards
        # (we're processing right-to-left, so we expand left from here)
        group_end = col
        group_start = col

        # Expand left to find the start of this group
        while group_start > 0:
            # Check if the column to the left is empty
            is_empty = op_line[group_start - 1] == ' ' and all(num_lines[row][group_start - 1] == ' ' for row in range(len(num_lines)))
            if is_empty:
                break
            group_start -= 1

        # Now extract numbers from this group, reading column-by-column right-to-left
        numbers = []
        for col_idx in range(group_end, group_start - 1, -1):
            # Extract the number in this column (reading top to bottom)
            num_str = ''.join(num_lines[row][col_idx] for row in range(len(num_lines)) if num_lines[row][col_idx].isdigit())
            if num_str:
                numbers.append(int(num_str))

        # Find the operation symbol for this group
        operation = None
        for col_idx in range(group_start, group_end + 1):
            if op_line[col_idx] in ['+', '*']:
                operation = op_line[col_idx]
                break

        # Calculate result
        if numbers and operation:
            result = numbers[0]
            for num in numbers[1:]:
                if operation == '+':
                    result += num
                elif operation == '*':
                    result *= num
            total += result

        # Move to the next group (left of current group)
        col = group_start - 1

    return total
