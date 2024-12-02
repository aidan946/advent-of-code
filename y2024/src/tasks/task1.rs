use std::fs;

pub fn sum_dif() {
    let contents =
        fs::read_to_string("inputs/task1.txt").expect("Something went wrong reading the file");
    let split = contents.split("\n");
    let mut list_1: Vec<i32> = Vec::new();
    let mut list_2: Vec<i32> = Vec::new();
    split.for_each(|x| {
        let mut split_line = x.split("   ");
        match parse_iter(split_line.next()) {
            Some(x) => list_1.push(x),
            None => (),
        };
        match parse_iter(split_line.next()) {
            Some(x) => list_2.push(x),
            None => (),
        };
    });
    list_1.sort();
    list_2.sort();

    let part1_sum = part_1(list_1.clone(), list_2.clone());
    let part2_sum = part_2(list_1.clone(), list_2.clone());
    println!("Part 1 sum: {}", part1_sum);
    println!("Part 2 sum: {}", part2_sum);
}

fn part_1(list_1: Vec<i32>, list_2: Vec<i32>) -> i32 {
    let mut dif_list: Vec<i32> = Vec::new();
    list_1.iter().zip(list_2.iter()).for_each(|(x, y)| {
        dif_list.push(match x - y {
            i if i < 0 => i * -1,
            i => i,
        });
    });
    let sum = dif_list.iter().sum::<i32>();
    return sum;
}

fn part_2(list_1: Vec<i32>, list_2: Vec<i32>) -> i32 {
    let mut sum_list: Vec<i32> = Vec::new();
    list_1.iter().for_each(|x| {
        let count = list_2.iter().filter(|&y| y == x).count();
        sum_list.push(x * count as i32);
    });
    let sum = sum_list.iter().sum::<i32>();
    return sum;
}

fn parse_iter(iter: Option<&str>) -> Option<i32> {
    match iter {
        Some(x) => match x.parse::<i32>() {
            Ok(x) => return Some(x),
            Err(_) => return None,
        },
        None => return None,
    }
}
