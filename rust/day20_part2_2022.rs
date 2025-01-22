
use std::fs;

#[derive(Clone)]
struct Num {
    pos: usize,
    val: i64,
}

fn mix(nums: &mut Vec<Num>) {
    let n = nums.len() - 1;
    for i in 0..nums.len() {
        let old_pos = nums[i].pos;
        let new_pos = (((old_pos as i64 + nums[i].val) % n as i64 + n as i64) % n as i64) as usize;
        if old_pos < new_pos {
            for j in 0..nums.len() {
                if nums[j].pos > old_pos && nums[j].pos <= new_pos {
                   nums[j].pos -= 1;
                }
            }
        }
        if new_pos < old_pos {
            for j in 0..nums.len() {
                if nums[j].pos >= new_pos && nums[j].pos < old_pos {
                   nums[j].pos += 1;
                }
            }
        }
        nums[i].pos = new_pos;
    }
}

fn coords(nums: &Vec<Num>) -> i64 {
    let l = nums.len();
    let zero_pos = nums.iter().find(|&num| num.val == 0).unwrap().pos;
    let mut sum = 0;
    for num in nums {
        if num.pos == (zero_pos + 1000) % l || num.pos == (zero_pos + 2000) % l || num.pos == (zero_pos + 3000) % l {
            sum += num.val;
        }
    }
    sum
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let nums: Vec<_> = input
        .lines()
        .enumerate()
        .map(|(i, n)| Num { pos: i, val: n.parse().unwrap() })
        .collect();
    let mut nums2: Vec<_> = nums
        .iter()
        .map(|num| Num { pos: num.pos, val: 811589153 * num.val })
        .collect();

    for _ in 0..10 {
        mix(&mut nums2);
    }

    println!("{}", coords(&nums2));
}
