
use std::fs::read_to_string;

fn bits(mut x: u64) -> u32 { x.count_ones() }

fn min_weight(mat: &mut Vec<Vec<u8>>, r: usize, c: usize) -> i32 {
    let mut col_pivot = vec![false; c];
    let mut piv = 0;
    for col in 0..c {
        if piv == r { break; }
        let sel = (piv..r).find(|&i| mat[i][col] == 1);
        if let Some(sr) = sel {
            mat.swap(piv, sr);
            for i in 0..r {
                if i != piv && mat[i][col] == 1 {
                    for k in col..=c {
                        mat[i][k] ^= mat[piv][k];
                    }
                }
            }
            col_pivot[col] = true;
            piv += 1;
        }
    }
    for row in &mat[piv..r] {
        if row[c] == 1 { return -1; }
    }
    let free: Vec<usize> = (0..c).filter(|&i| !col_pivot[i]).collect();
    let n_free = free.len();
    let mut best = i32::MAX;
    let limit = 1u64 << n_free;
    for mask in 0..limit {
        let mut x = vec![0u8; c];
        let mut w = bits(mask) as i32;
        for (j, &f) in free.iter().enumerate() {
            if mask >> j & 1 == 1 { x[f] = 1; }
        }
        let mut prow = 0;
        for col in 0..c {
            if col_pivot[col] {
                let mut v = mat[prow][c];
                for k in (col+1)..c {
                    if mat[prow][k] == 1 { v ^= x[k]; }
                }
                x[col] = v;
                if v == 1 { w += 1; }
                prow += 1;
            }
        }
        if w < best { best = w; }
    }
    best
}

fn main() {
    let data = read_to_string("input.txt").unwrap();
    let mut total = 0;
    for line in data.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
        let lb = line.find('[').unwrap_or(usize::MAX);
        let rb = line.rfind(']').unwrap_or(0);
        if lb >= rb { continue; }
        let target_str = &line[lb+1..rb];
        let r = target_str.len();
        let target: Vec<u8> = target_str.chars().map(|c| if c=='#' {1} else {0}).collect();
        let rest = &line[rb+1..];
        let btn_matches: Vec<&str> = rest.split("(").filter_map(|s| {
            let end = s.find(')')?;
            Some(&s[..end])
        }).collect();
        let c = btn_matches.len();
        let mut matrix = vec![vec![0u8; c+1]; r];
        for (i, &btn_str) in btn_matches.iter().enumerate() {
            if !btn_str.is_empty() {
                for num in btn_str.split(',') {
                    let idx: usize = num.trim().parse().unwrap();
                    matrix[idx][i] = 1;
                }
            }
        }
        for row in 0..r { matrix[row][c] = target[row]; }
        let mw = min_weight(&mut matrix, r, c);
        if mw != -1 { total += mw; }
    }
    println!("{}", total);
}
