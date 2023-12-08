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


def part1(network: Tuple[str, Dict[str, Tuple[str, str]]]):
    sequence, nodes = network
    current = "AAA"
    steps = 0
    for next_dir in cycle(sequence):
        steps += 1
        if next_dir == "L":
            current = nodes[current][0]
        elif next_dir == "R":
            current = nodes[current][1]
        if current == "ZZZ":
            return steps
