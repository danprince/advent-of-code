extern crate itertools;

use itertools::Itertools;
use std::collections::HashMap;
use std::fs;

type Slope = (i32, i32);

#[derive(Debug, Hash, PartialEq, Eq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    fn distance(&self, other: &Point) -> f64 {
        let dy = self.y - other.y;
        let dx = self.y - other.y;
        ((dy.pow(2) + dx.pow(2)) as f64).sqrt()
    }

    fn slope(&self, other: &Point) -> Slope {
        let dy = self.y - other.y;
        let dx = self.x - other.x;

        let mut m = dy;
        let mut n = dx;

        while m != 0 {
            let temp = m;
            m = n % temp;
            n = temp;
        }

        let gcd = n.abs();

        (dy / gcd, dx / gcd)
    }

}

fn main() {
    let input = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    // Create a vector of points representing the location of each
    // asteroid.

    let asteroids: Vec<Point> = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line
                .chars()
                .enumerate()
                .filter(|(_, c)| c == &'#')
                .map(move |(x, _)| Point::new(x as i32, y as i32))
        })
        .collect();

    // Create a map of asteroid positions to visibility maps that group
    // the other asteroids that can be seen, by the slope of the line on
    // which they can be seen.

    let visibility_maps: HashMap<_, _> = asteroids
        .iter()
        .map(|origin| {
            let mut map = asteroids
                .iter()
                .filter(|target| target != &origin)
                .map(|target| (origin.slope(target), target))
                .into_group_map();

            for group in map.values_mut() {
                group.sort_by(|t1, t2| {
                    let d1 = origin.distance(t1);
                    let d2 = origin.distance(t2);
                    d1.partial_cmp(&d2).unwrap()
                });
            }

            (origin, map)
        })
        .collect();

    let (_, visibility_map) = visibility_maps
        .iter()
        .max_by_key(|(_, map)| map.len())
        .unwrap();

    println!("Part 1: {}", visibility_map.len());

    // TODO: Can't figure out how to implement part 2 in rust yet.
    // Leaving some notes for future reference.
    //
    // Create a ring buffer of slopes in order of their angle (with 0
    // being directly upwards from the origin).
    //
    // For each slope in the ring, keep a queue/stack of asteroids that
    // can be found at the corresponding slope, sorted by distance from
    // the monitoring station.
    //
    // To solve the problem, cycle on the ring buffer and for each
    // slope, pop the top item off the stack and mark it as destroyed.
}
