from dataclasses import dataclass
from typing import Tuple, Dict, List


@dataclass
class Condition:
    lhs: str
    rel: str
    rhs: int
    then: str


Config = Dict[str, int]


@dataclass
class System:
    workflows: Dict[str, List[Condition | str]]

    def run(self, config: Config, workflow_name: str) -> bool:
        if workflow_name == "A":
            return True
        elif workflow_name == "R":
            return False
        workflow = self.workflows[workflow_name]
        for step in workflow:
            if isinstance(step, Condition):
                lhs = config[step.lhs]
                if step.rel == ">":
                    cond = lhs > step.rhs
                else:
                    cond = lhs < step.rhs
                if cond:
                    return self.run(config, step.then)
                continue
            else:
                return self.run(config, step)
        raise RuntimeError("Unreachable")


def parse(lines: str) -> Tuple[System, List[Config]]:
    def parse_rule(rule: str) -> Condition | str:
        if ":" in rule:
            cond, then = rule.split(":")
            lhs = cond[0]
            rel = cond[1]
            rhs = int(cond[2:])
            return Condition(lhs=lhs, rel=rel, rhs=rhs, then=then)
        return rule

    def parse_workflow(workflow: str) -> Tuple[str, List[Condition | str]]:
        name, rest = workflow[:-1].split("{")
        return name, [*map(parse_rule, rest.split(","))]

    def parse_entry(entry: str) -> Tuple[str, int]:
        key, val_raw = entry.split("=")
        return key, int(val_raw)

    def parse_config(config: str) -> Config:
        entries = config[1:-1].split(",")
        return dict(map(parse_entry, entries))

    workflows_raw, configs_raw = lines.split("\n\n")
    workflows = dict(map(parse_workflow, workflows_raw.split("\n")))
    configs = [*map(parse_config, configs_raw.split("\n"))]
    return System(workflows=workflows), configs


def part1(inputs: Tuple[System, List[Config]]) -> int:
    results = 0
    workflows, configs = inputs
    for config in configs:
        if workflows.run(config, "in"):
            results += sum(config.values())
    return results
