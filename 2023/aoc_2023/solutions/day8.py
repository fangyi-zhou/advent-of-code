from typing import Dict, Tuple
from itertools import cycle


def parse(lines: str) -> Tuple[str, Dict[str, Tuple[str, str]]]:
    def parse_node(node: str) -> Tuple[str, str, str]:
        node_name, node_nexts = node.split("=")
        node_left, node_right = node_nexts.split(",")
        return node_name.strip(), node_left.strip()[1:], node_right.strip()[:-1]

    sequence, _, *nodes = lines.split("\n")
    nodes_map: Dict[str, Tuple[str, str]] = {}
    for node_name, node_left, node_right in map(parse_node, nodes):
        nodes_map[node_name] = (node_left, node_right)
    return sequence, nodes_map


def _step(nodes: Dict[str, Tuple[str, str]], current: str, direction: str) -> str:
    if direction == "L":
        return nodes[current][0]
    elif direction == "R":
        return nodes[current][1]
    raise RuntimeError("Invalid direction " + direction)


def part1(network: Tuple[str, Dict[str, Tuple[str, str]]]) -> int:
    sequence, nodes = network
    current = "AAA"
    steps = 0
    for next_dir in cycle(sequence):
        steps += 1
        current = _step(nodes, current, next_dir)
        if current == "ZZZ":
            return steps
    raise RuntimeError("Unreachable")


# def part2(network: Tuple[str, Dict[str, Tuple[str, str]]]):
#     sequence, nodes = network
#     currents = [*filter(lambda n: n[-1] == "A", nodes)]
#     steps = 0
#     for next_dir in cycle(sequence):
#         steps += 1
#         currents = [*map(lambda current: _step(nodes, current, next_dir), currents)]
#         if all(n[-1] == "Z" for n in currents):
#             return steps
