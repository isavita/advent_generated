
use std::fs;

#[derive(Debug)]
struct FileSegment {
    id: usize,
    start: usize,
    end: usize,
}

fn solve(disk: &mut Vec<Option<usize>>) -> usize {
    let mut files = Vec::new();
    let mut curr_id: Option<usize> = None;
    let mut start = 0;

    for (i, &val) in disk.iter().enumerate() {
        match val {
            None => {
                curr_id = None;
            }
            Some(file_id) => {
                if curr_id != Some(file_id) {
                    curr_id = Some(file_id);
                    start = i;
                }

                if i == disk.len() - 1 || disk[i + 1] != Some(file_id) {
                    files.push(FileSegment {
                        id: file_id,
                        start,
                        end: i,
                    });
                }
            }
        }
    }

    for file in files.iter().rev() {
        let file_len = file.end - file.start + 1;
        let mut leftmost_span = -1;
        let mut span_len = 0;

        for i in 0..file.start {
            if disk[i].is_none() {
                if span_len == 0 {
                    leftmost_span = i as i32;
                }
                span_len += 1;
                if span_len == file_len {
                    break;
                }
            } else {
                span_len = 0;
                leftmost_span = -1;
            }
        }

        if leftmost_span != -1 && span_len == file_len {
            for i in file.start..=file.end {
                disk[i] = None;
            }
            for i in 0..file_len {
                disk[leftmost_span as usize + i] = Some(file.id);
            }
        }
    }

    let mut checksum = 0;
    for (i, &val) in disk.iter().enumerate() {
        if let Some(file_id) = val {
            checksum += i * file_id;
        }
    }
    checksum
}

fn main() {
    let line = fs::read_to_string("input.txt").unwrap();
    let line = line.trim();

    let mut disk: Vec<Option<usize>> = Vec::new();
    let mut file_id = 0;
    let mut is_file = true;

    for char in line.chars() {
        let length = char.to_digit(10).unwrap() as usize;
        if is_file {
            disk.extend(std::iter::repeat(Some(file_id)).take(length));
            file_id += 1;
        } else {
            disk.extend(std::iter::repeat(None).take(length));
        }
        is_file = !is_file;
    }

    let checksum = solve(&mut disk);
    println!("{}", checksum);
}
