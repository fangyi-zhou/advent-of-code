import importlib
import os
import sys

import requests

if __name__ == "__main__":
    # TODO: Use argparse
    day = sys.argv[1]
    year = 2023
    solution = importlib.import_module("solutions.day" + day)
    input_file = f"inputs/{day}.txt"
    if not os.path.exists(input_file) and len(sys.argv) <= 2:
        with open(".session") as f:
            session = f.read().strip()
        r = requests.get(
            f"https://adventofcode.com/{year}/day/{day}/input",
            cookies={"session": session},
            headers={"User-Agent": "github.com/fangyi-zhou me+aoc@fangyi.io"},
        )
        if r.status_code != 200:
            raise RuntimeError("failed to download input", r.text)
        input_data = r.text.strip()
        with open(input_file, "w") as f:
            f.write(r.text)
    else:
        input_file = sys.argv[2] if len(sys.argv) > 2 else input_file
        with open(input_file) as f:
            input_data = f.read().strip()
    parsed = solution.parse(input_data)
    if hasattr(solution, "part1"):
       print(solution.part1(parsed))
    if hasattr(solution, "part2"):
        print(solution.part2(parsed))
