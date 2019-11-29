# http://adventofcode.com/2017/day/7

from part_one import parse_programs, find_root

def programs_to_tree(programs):
    return { program["id"]: program for program in programs }

def calculate_weights(tree, id):
    program = tree[id]
    children = program["children"]
    weights = [calculate_weights(tree, child) for child in children]
    program["total"] = program["weight"] + sum(weights)
    return program["total"]

def find_imbalance(tree, id, target=0):
    seen = {}
    new_target = 0
    program = tree[id]

    for id in program["children"]:
        child = tree[id]
        total = child["total"]

        if total in seen:
            seen[total] = False
            new_target = total
        else:
            seen[total] = id

    for id in seen.values():
        if id is not False:
            return find_imbalance(tree, id, new_target)

    return program["weight"] + target - program["total"]

def solve():
    with open("input.txt") as file:
        data = file.read()
        programs = parse_programs(data)
        root = find_root(programs)
        tree = programs_to_tree(programs)
        calculate_weights(tree, root)
        return find_imbalance(tree, root)

