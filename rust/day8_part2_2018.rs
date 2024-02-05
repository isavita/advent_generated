
use std::fs;

fn parse_input(input: &str) -> (Vec<i32>, usize) {
    let mut nums = Vec::new();
    for num in input.trim().split_whitespace() {
        nums.push(num.parse().unwrap());
    }
    (nums, 0)
}

fn sum_metadata(nums: &mut Vec<i32>, index: &mut usize) -> i32 {
    let num_child_nodes = nums[*index];
    let num_metadata = nums[*index + 1];
    *index += 2;
    let mut sum = 0;
    for _ in 0..num_child_nodes {
        sum += sum_metadata(nums, index);
    }
    for _ in 0..num_metadata {
        sum += nums[*index];
        *index += 1;
    }
    sum
}

fn calculate_node_value(nums: &mut Vec<i32>, index: &mut usize) -> i32 {
    let num_child_nodes = nums[*index];
    let num_metadata = nums[*index + 1];
    *index += 2;
    let mut child_values = Vec::new();
    for _ in 0..num_child_nodes {
        child_values.push(calculate_node_value(nums, index));
    }
    if num_child_nodes == 0 {
        let mut sum = 0;
        for _ in 0..num_metadata {
            sum += nums[*index];
            *index += 1;
        }
        sum
    } else {
        let mut sum = 0;
        for _ in 0..num_metadata {
            let metadata = nums[*index] as usize - 1;
            if metadata < child_values.len() {
                sum += child_values[metadata];
            }
            *index += 1;
        }
        sum
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let (mut nums, mut index) = parse_input(&input);
    
    let sum_metadata_entries = sum_metadata(&mut nums, &mut index);
    println!("{}", sum_metadata_entries);

    let mut index = 0;
    let root_node_value = calculate_node_value(&mut nums, &mut index);
    println!("{}", root_node_value);
}
