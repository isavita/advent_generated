
use std::fs::File;
use std::io::{self, BufRead};

const MOD: u64 = 1 << 24;
const NUM_STEPS: usize = 2000;

fn next_secret(s: u64) -> u64 {
    let mut s = s;
    s ^= s * 64;
    s &= MOD - 1;
    s ^= s / 32;
    s &= MOD - 1;
    s ^= s * 2048;
    s &= MOD - 1;
    s
}

fn encode_change4(c1: i32, c2: i32, c3: i32, c4: i32) -> usize {
    ((c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19) as usize
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut initials: Vec<u64> = Vec::new();
    for line in reader.lines() {
        let line = line?;
        if !line.is_empty() {
          initials.push(line.parse().expect("failed to parse number"));
        }
    }

    let num_buyers = initials.len();
    let mut all_prices: Vec<Vec<i32>> = Vec::with_capacity(num_buyers);
    
    for init_val in &initials{
        let mut prices = Vec::with_capacity(NUM_STEPS + 1);
        let mut s = *init_val;
        for _ in 0..=NUM_STEPS{
           prices.push((s % 10) as i32);
           s = next_secret(s);
        }
        all_prices.push(prices);
    }

    let mut global_sum: Vec<i64> = vec![0; 19 * 19 * 19 * 19];

    for prices in &all_prices{
        let mut local_price: Vec<i32> = vec![-1; 19 * 19 * 19 * 19];

        let changes : Vec<i32> = prices.windows(2).map(|w| w[1] - w[0]).collect();

        for i in 0..changes.len() - 3{
            let c1 = changes[i];
            let c2 = changes[i+1];
            let c3 = changes[i+2];
            let c4 = changes[i+3];

            if c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 || c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9{
                continue;
            }

            let idx = encode_change4(c1, c2, c3, c4);

            if local_price[idx] < 0{
                 local_price[idx] = prices[i+4];
            }
        }
        for (idx, &p) in local_price.iter().enumerate(){
            if p >= 0 {
                global_sum[idx] += p as i64;
            }
        }
    }

    let ans = global_sum.iter().max().unwrap_or(&0);
    println!("{}",ans);
    
    Ok(())
}
