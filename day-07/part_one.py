# http://adventofcode.com/2017/day/7

import re

PROGRAM_REGEX = "(\w+) \((\d+)\) ?-?>? ?(.*)";

def parse_program(str):
    match = re.search(PROGRAM_REGEX, str)
    id, weight, children = match.groups()

    return {
        "id": id,
        "weight": int(weight),
        "children": [] if children == "" else children.split(", ")
    }

def parse_programs(str):
    rows = str.strip().split("\n")
    return [parse_program(row) for row in rows]

def find_root(programs):
    child_programs = set()

    for program in programs:
        for child in program["children"]:
            child_programs.add(child)

    for program in programs:
        if program["id"] not in child_programs:
            return program["id"]

def solve():
    with open("input.txt") as file:
        data = file.read()
        programs = parse_programs(data)
        return find_root(programs)

