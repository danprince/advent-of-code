use std::collections::HashMap;
use std::fs;

fn main() {
    let src = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let data = src
        .chars()
        .map(|c| c.to_string().parse::<i32>().unwrap())
        .collect();

    let img = SpaceImage::new(25, 6, data);

    println!("Part 1: {}", part_one(&img));
    println!("Part 2: \n{}", part_two(&img));
}

fn part_one(img: &SpaceImage) -> u32 {
    (0..img.depth)
        .map(|z| img.get_layer_frequencies(z))
        .min_by(|stats_a, stats_b| {
            let a = stats_a.get(&0).unwrap();
            let b = stats_b.get(&0).unwrap();
            a.cmp(b)
        })
        .map(|stats| {
            let ones = stats.get(&1).unwrap();
            let twos = stats.get(&2).unwrap();
            ones * twos
        })
        .unwrap()
}

fn part_two(img: &SpaceImage) -> String {
    let mut data: Vec<Vec<i32>> = vec![vec![0; img.width]; img.height];

    for y in 0..img.height {
        for x in 0..img.width {
            data[y][x] = (0..img.depth)
                .find_map(|z| match img.get_pixel(x, y, z) {
                    Some(pixel) if pixel < 2 => Some(pixel),
                    _ => None,
                })
                .unwrap()
        }
    }

    data.iter()
        .map(|row| {
            row.iter()
                .map(|c| match c {
                    1 => "#",
                    _ => " ",
                })
                .collect::<Vec<_>>()
                .join("")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(Debug)]
struct SpaceImage {
    width: usize,
    height: usize,
    depth: usize,
    data: Vec<i32>,
}

impl SpaceImage {
    fn new(width: usize, height: usize, data: Vec<i32>) -> SpaceImage {
        SpaceImage {
            width,
            height,
            depth: data.len() / (width * height),
            data,
        }
    }

    fn get_pixel(&self, x: usize, y: usize, z: usize) -> Option<i32> {
        if x < self.width || y < self.height || z < self.depth {
            Some(self.data[x + (y * self.width) + (z * self.width * self.height)])
        } else {
            None
        }
    }

    fn get_layer_frequencies(&self, z: usize) -> HashMap<i32, u32> {
        let mut stats = HashMap::new();

        for y in 0..self.height {
            for x in 0..self.width {
                let pixel = self.get_pixel(x, y, z).unwrap();

                stats
                    .entry(pixel)
                    .and_modify(|count| *count += 1)
                    .or_insert(1);
            }
        }

        stats
    }
}
