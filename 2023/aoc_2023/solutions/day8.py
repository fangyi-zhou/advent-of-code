from typing import Dict, Tuple
import math
from itertools import cycle


def parse(lines: str) -> Tuple[str, Dict[str, Tuple[str, str]]]:
    def parse_node(node: str) -> Tuple[str, str, str]:
        node_name, node_nexts = node.split("=")
        node_left, node_right = node_nexts.split(",")
        return node_name.strip(), node_left.strip()[1:], node_right.strip()[:-1]

    sequence, _, *nodes = lines.split("\n")
    nodes_map: Dict[str, Tuple[str, str]] = {
        node_name: (node_left, node_right)
        for node_name, node_left, node_right in map(parse_node, nodes)
    }
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
    for steps, next_dir in enumerate(cycle(sequence)):
        current = _step(nodes, current, next_dir)
        if current == "ZZZ":
            return steps + 1
    raise RuntimeError("Unreachable")


def part2(network: Tuple[str, Dict[str, Tuple[str, str]]]) -> int:
    sequence, nodes = network
    currents = set(filter(lambda n: n[-1] == "A", nodes))
    steps_done = []
    for steps, next_dir in enumerate(cycle(sequence)):
        currents = set(map(lambda current: _step(nodes, current, next_dir), currents))
        done = set(filter(lambda n: n[-1] == "Z", currents))
        if done:
            steps_done.append(steps + 1)
            currents = currents - done
        if not currents:
            break
    return math.lcm(*steps_done)
