
module main

import os

fn mix(secret u64, value u64) u64 {
    return secret ^ value
}

fn prune(secret u64) u64 {
    return secret % 16777216
}

fn next_secret(secret u64) u64 {
    mut result := secret
    
    // Step 1: multiply by 64, mix, prune
    result = mix(result, result * 64)
    result = prune(result)
    
    // Step 2: divide by 32, mix, prune
    result = mix(result, result / 32)
    result = prune(result)
    
    // Step 3: multiply by 2048, mix, prune
    result = mix(result, result * 2048)
    result = prune(result)
    
    return result
}

fn generate_nth_secret(initial u64, n int) u64 {
    mut secret := initial
    for _ in 0 .. n {
        secret = next_secret(secret)
    }
    return secret
}

fn main() {
    // Read input from file
    content := os.read_file('input.txt') or {
        eprintln('Failed to read input.txt')
        exit(1)
    }
    
    // Parse the numbers
    lines := content.trim_space().split_into_lines()
    mut total := u64(0)
    
    for line in lines {
        if line.len == 0 {
            continue
        }
        initial := line.u64()
        // Generate the 2000th new secret number (index 2000)
        nth_secret := generate_nth_secret(initial, 2000)
        total += nth_secret
    }
    
    println(total)
}
