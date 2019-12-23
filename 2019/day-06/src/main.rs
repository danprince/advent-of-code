use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::i32;

struct OrbitalMap {
    // Mapping of objects to their children
    child_index: HashMap<String, Vec<String>>,

    // Mapping of objects to their parent
    parent_index: HashMap<String, String>,
}

impl OrbitalMap {
    fn new() -> OrbitalMap {
        OrbitalMap {
            child_index: HashMap::new(),
            parent_index: HashMap::new(),
        }
    }

    fn insert(&mut self, parent: &str, child: &str) {
        self.child_index
            .entry(parent.to_string())
            .or_insert_with(|| Vec::new())
            .push(child.to_string());

        self.parent_index
            .insert(child.to_string(), parent.to_string());
    }

    fn get_children(&self, object: &str) -> Option<&Vec<String>> {
        self.child_index.get(object)
    }

    fn get_parent(&self, object: &str) -> Option<&String> {
        self.parent_index.get(object)
    }

    fn parse(input: &str) -> OrbitalMap {
        let mut map = OrbitalMap::new();

        for line in input.lines() {
            let mut parts = line.splitn(2, ')');

            if let (Some(parent), Some(child)) = (parts.next(), parts.next()) {
                map.insert(parent, child);
            }
        }

        map
    }

    // TODO: Would be more efficient with Dijkstra map and priority queue
    fn to_distance_map(&self, initial: String) -> HashMap<&String, i32> {
        let mut stack: Vec<&String> = vec![&initial];
        let mut distances: HashMap<&String, i32> = HashMap::new();
        let mut visited: HashSet<&String> = HashSet::new();

        while !stack.is_empty() {
            let object = stack.pop().unwrap();
            let current_distance = *distances.get(&object).unwrap_or(&0);

            visited.insert(&object);

            if let Some(children) = self.get_children(&object) {
                for child in children {
                    if !visited.contains(&child) {
                        distances.insert(child, current_distance + 1);
                        stack.push(child);
                    }
                }
            }

            if let Some(parent) = self.get_parent(&object) {
                if !visited.contains(&parent) {
                    distances.insert(parent, current_distance + 1);
                    stack.push(parent);
                }
            }
        }

        distances
    }
}

fn main() {
    let input = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let map = OrbitalMap::parse(&input);

    println!("Part 1: {}", part_one(&map));
    println!("Part 2: {}", part_two(&map));
}

fn part_one(map: &OrbitalMap) -> i32 {
    let initial = String::from("COM");
    let distances = map.to_distance_map(initial);
    distances.values().sum()
}

fn part_two(map: &OrbitalMap) -> i32 {
    let initial = map.get_parent("YOU").unwrap().to_string();
    let target = map.get_parent("SAN").unwrap().to_string();
    let distances = map.to_distance_map(initial);
    *distances.get(&target).expect("No path to target!")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        let map = OrbitalMap::parse(
            "
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L",
        );
        assert_eq!(part_one(&map), 42);
    }

    #[test]
    fn part_two_examples() {
        let map = OrbitalMap::parse(
            "
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN",
        );

        assert_eq!(part_two(&map), 4);
    }
}
