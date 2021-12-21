use regex::Regex;
use std::{fs::read_to_string, ops::Range};

#[derive(Debug, Clone, Copy)]
struct Coord(isize, isize);

#[derive(Debug, Clone)]
struct TargetArea(pub Range<isize>, pub Range<isize>);
impl TargetArea {
    fn contains(&self, c: Coord) -> bool {
        self.0.contains(&c.0) && self.1.contains(&c.1)
    }
}

fn parse_target_area(line: &str) -> TargetArea {
    let x_caps = Regex::new(r"x=([-\d]*)..([-\d]*)")
        .unwrap()
        .captures(line)
        .unwrap();
    let y_caps = Regex::new(r"y=([-\d]*)..([-\d]*)")
        .unwrap()
        .captures(line)
        .unwrap();

    TargetArea(
        x_caps[1].parse().unwrap()..x_caps[2].parse::<isize>().unwrap() + 1isize,
        y_caps[1].parse().unwrap()..y_caps[2].parse::<isize>().unwrap() + 1isize,
    )
}

fn does_intersect(mut vx: isize, mut vy: isize, area: &TargetArea) -> bool {
    let mut x = 0;
    let mut y = 0;
    for _ in 0.. {
        x += vx;
        y += vy;

        vx = if vx > 0 {
            vx - 1
        } else if vx < 0 {
            vx + 1
        } else {
            0
        };
        vy -= 1;
        if area.contains(Coord(x, y)) {
            return true;
        } else if x > area.0.end || y < area.1.start {
            return false;
        }
    }

    unreachable!()
}

fn max_y(mut vy: isize) -> isize {
    let mut y = 0;
    for _ in 0.. {
        y += vy;
        vy -= 1;

        if vy <= 0 {
            return y;
        }
    }

    unreachable!()
}

fn main() {
    let area = parse_target_area(&read_to_string("./input").unwrap());

    let mut max_y_found = 0;
    let mut max_y_vel = Coord(0, 0);
    let mut num_valid = 0;
    for vx in 0..=area.0.end * 2 {
        for vy in -area.1.start.abs() * 2..=area.1.start.abs() * 2 {
            if does_intersect(vx, vy, &area) {
                if max_y(vy) > max_y_found {
                    max_y_found = max_y(vy);
                    max_y_vel = Coord(vx, vy);
                }
                num_valid += 1;
            }
        }
    }

    println!("part 1: {} with velocity {:?}", max_y_found, max_y_vel);
    println!("part 2: {} possible velocities", num_valid);
}
