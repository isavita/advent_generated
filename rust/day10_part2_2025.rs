
use std::fs::File;
use std::io::{BufRead, BufReader};

const MAX_COUNTERS: usize = 20;
const MAX_BUTTONS: usize = 50;
const INF: i32 = 0x3f3f3f3f;

static mut BUTTONS: [[i32; MAX_COUNTERS]; MAX_BUTTONS] = [[0; MAX_COUNTERS]; MAX_BUTTONS];
static mut BTN_SIZE: [usize; MAX_BUTTONS] = [0; MAX_BUTTONS];
static mut TARGETS: [i32; MAX_COUNTERS] = [0; MAX_COUNTERS];
static mut NUM_COUNTERS: usize = 0;
static mut NUM_BUTTONS: usize = 0;

static mut MATRIX: [[f64; MAX_BUTTONS + 1]; MAX_COUNTERS] = [[0.0; MAX_BUTTONS + 1]; MAX_COUNTERS];
static mut PIVOT_COL: [i32; MAX_COUNTERS] = [-1; MAX_COUNTERS];
static mut IS_PIVOT: [bool; MAX_BUTTONS] = [false; MAX_BUTTONS];
static mut PIVOT_ROWS: [i32; MAX_BUTTONS] = [-1; MAX_BUTTONS];
static mut FREE_VARS: [usize; MAX_BUTTONS] = [0; MAX_BUTTONS];
static mut NUM_FREE: usize = 0;
static mut MAX_PRESSES: [i32; MAX_BUTTONS] = [0; MAX_BUTTONS];
static mut FREE_VALUES: [i32; MAX_BUTTONS] = [0; MAX_BUTTONS];
static mut BEST_RESULT: i32 = INF;

unsafe fn parse_line(line: &str) {
    NUM_COUNTERS = 0;
    NUM_BUTTONS = 0;
    let mut chars = line.chars().peekable();
    while let Some(&c) = chars.peek() {
        if c == '(' {
            chars.next();
            let btn_idx = NUM_BUTTONS;
            NUM_BUTTONS += 1;
            BTN_SIZE[btn_idx] = 0;
            while let Some(&c) = chars.peek() {
                if c == ')' {
                    chars.next();
                    break;
                }
                if c.is_ascii_digit() {
                    let mut x = 0;
                    while let Some(&d) = chars.peek() {
                        if d.is_ascii_digit() {
                            x = x * 10 + (d as u8 - b'0') as i32;
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    BUTTONS[btn_idx][BTN_SIZE[btn_idx]] = x;
                    BTN_SIZE[btn_idx] += 1;
                } else {
                    chars.next();
                }
            }
        } else if c == '{' {
            chars.next();
            while let Some(&c) = chars.peek() {
                if c == '}' {
                    break;
                }
                if c.is_ascii_digit() {
                    let mut x = 0;
                    while let Some(&d) = chars.peek() {
                        if d.is_ascii_digit() {
                            x = x * 10 + (d as u8 - b'0') as i32;
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    TARGETS[NUM_COUNTERS] = x;
                    NUM_COUNTERS += 1;
                } else {
                    chars.next();
                }
            }
            break;
        } else {
            chars.next();
        }
    }
}

unsafe fn gauss() {
    for j in 0..NUM_COUNTERS {
        for i in 0..=NUM_BUTTONS {
            MATRIX[j][i] = 0.0;
        }
        MATRIX[j][NUM_BUTTONS] = TARGETS[j] as f64;
    }
    for i in 0..NUM_BUTTONS {
        for j in 0..BTN_SIZE[i] {
            let c = BUTTONS[i][j] as usize;
            if c < NUM_COUNTERS {
                MATRIX[c][i] = 1.0;
            }
        }
    }

    for i in 0..NUM_COUNTERS {
        PIVOT_COL[i] = -1;
    }

    let mut row = 0;
    for col in 0..NUM_BUTTONS {
        if row >= NUM_COUNTERS {
            break;
        }
        let mut max_row = row;
        for r in (row + 1)..NUM_COUNTERS {
            if MATRIX[r][col].abs() > MATRIX[max_row][col].abs() {
                max_row = r;
            }
        }
        if MATRIX[max_row][col].abs() < 1e-9 {
            continue;
        }

        for c in 0..=NUM_BUTTONS {
            let tmp = MATRIX[row][c];
            MATRIX[row][c] = MATRIX[max_row][c];
            MATRIX[max_row][c] = tmp;
        }
        let scale = MATRIX[row][col];
        for c in col..=NUM_BUTTONS {
            MATRIX[row][c] /= scale;
        }

        for r in 0..NUM_COUNTERS {
            if r != row && MATRIX[r][col].abs() > 1e-9 {
                let factor = MATRIX[r][col];
                for c in col..=NUM_BUTTONS {
                    MATRIX[r][c] -= factor * MATRIX[row][c];
                }
            }
        }
        PIVOT_COL[row] = col as i32;
        row += 1;
    }

    let rank = row;
    for i in 0..NUM_BUTTONS {
        IS_PIVOT[i] = false;
        PIVOT_ROWS[i] = -1;
    }
    for r in 0..rank {
        let c = PIVOT_COL[r];
        if c >= 0 {
            let c = c as usize;
            IS_PIVOT[c] = true;
            PIVOT_ROWS[c] = r as i32;
        }
    }
    NUM_FREE = 0;
    for i in 0..NUM_BUTTONS {
        if !IS_PIVOT[i] {
            FREE_VARS[NUM_FREE] = i;
            NUM_FREE += 1;
        }
    }

    for i in 0..NUM_BUTTONS {
        MAX_PRESSES[i] = INF;
        for j in 0..BTN_SIZE[i] {
            let c = BUTTONS[i][j] as usize;
            if c < NUM_COUNTERS && TARGETS[c] < MAX_PRESSES[i] {
                MAX_PRESSES[i] = TARGETS[c];
            }
        }
        if MAX_PRESSES[i] == INF {
            MAX_PRESSES[i] = 0;
        }
    }

    for i in 0..NUM_FREE {
        for j in (i + 1)..NUM_FREE {
            if MAX_PRESSES[FREE_VARS[i]] > MAX_PRESSES[FREE_VARS[j]] {
                FREE_VARS.swap(i, j);
            }
        }
    }
}

unsafe fn compute_pivots(presses: &mut [i32]) -> i32 {
    for i in 0..NUM_BUTTONS {
        presses[i] = 0;
    }

    for i in 0..NUM_FREE {
        presses[FREE_VARS[i]] = FREE_VALUES[i];
    }

    for r in (0..NUM_COUNTERS).rev() {
        let col = PIVOT_COL[r];
        if col < 0 {
            continue;
        }
        let col = col as usize;
        let mut val = MATRIX[r][NUM_BUTTONS];
        for c in (col + 1)..NUM_BUTTONS {
            val -= MATRIX[r][c] * presses[c] as f64;
        }

        let int_val = val.round() as i32;
        if (val - int_val as f64).abs() > 1e-6 {
            return 0;
        }
        if int_val < 0 {
            return 0;
        }
        if int_val > MAX_PRESSES[col] {
            return 0;
        }
        presses[col] = int_val;
    }

    let sum: i32 = presses.iter().take(NUM_BUTTONS).sum();
    sum
}

unsafe fn enumerate(idx: usize, current_sum: i32) {
    if current_sum >= BEST_RESULT {
        return;
    }
    if idx == NUM_FREE {
        let mut presses = [0i32; MAX_BUTTONS];
        let sum = compute_pivots(&mut presses);
        if sum > 0 && sum < BEST_RESULT {
            BEST_RESULT = sum;
        }
        return;
    }
    let fv = FREE_VARS[idx];
    let max_val = MAX_PRESSES[fv];
    for v in 0..=max_val {
        FREE_VALUES[idx] = v;
        enumerate(idx + 1, current_sum + v);
    }
}

unsafe fn solve() -> i32 {
    gauss();
    BEST_RESULT = INF;
    enumerate(0, 0);
    if BEST_RESULT == INF {
        -1
    } else {
        BEST_RESULT
    }
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut total = 0;
    for line in reader.lines().flatten() {
        if line.trim().is_empty() {
            continue;
        }
        unsafe {
            parse_line(&line);
            let res = solve();
            if res > 0 {
                total += res;
            }
        }
    }
    println!("{}", total);
}
