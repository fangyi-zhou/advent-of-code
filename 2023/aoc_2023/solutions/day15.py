from typing import List, Optional, Tuple


def parse(sequence: str) -> List[str]:
    return sequence.split(",")


def part1(sequence: List[str]) -> int:
    result = 0
    for step in sequence:
        curr = 0
        for ch in step:
            curr = ((curr + ord(ch)) * 17) % 256
            # print(ch, ord(ch), curr)
        result += curr
    return result


def _set(lst: List[Tuple[str, int]], key: str, val: int) -> None:
    for idx, (k, _) in enumerate(lst):
        if k == key:
            lst[idx] = (k, val)
            return
    lst.append((key, val))


def _remove(lst: List[Tuple[str, int]], key: str) -> None:
    for idx, (k, _) in enumerate(lst):
        if k == key:
            del lst[idx]
            return


def part2(sequence: List[str]) -> int:
    result = 0
    boxes: List[List[Tuple[str, int]]] = [[] for _ in range(256)]
    for step in sequence:
        arg: Optional[int]
        if "-" in step:
            name = step[:-1]
            arg = None
        else:
            name, arg_s = step.split("=")
            arg = int(arg_s)
        hash_val = 0
        for ch in name:
            hash_val = ((hash_val + ord(ch)) * 17) % 256
        if arg:
            _set(boxes[hash_val], name, arg)
        else:
            _remove(boxes[hash_val], name)
    for box_id, box in enumerate(boxes):
        for slot_id, (_, focal) in enumerate(box):
            result += (box_id + 1) * (slot_id + 1) * focal
    return result
