
use std::fs::File;
use std::io::Read;

const K: [u32; 64] = [
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
];

const R: [u32; 64] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15,
    21, 6, 10, 15, 21, 6, 10, 15, 21,
];

fn left_rotate(x: u32, c: u32) -> u32 {
    (x << c) | (x >> (32 - c))
}

fn md5_transform(state: &mut [u32; 4], block: &[u32; 16]) {
    let mut a = state[0];
    let mut b = state[1];
    let mut c = state[2];
    let mut d = state[3];

    for i in 0..64 {
        let (f, g) = if i < 16 {
            ((b & c) | (!b & d), i)
        } else if i < 32 {
            ((d & b) | (!d & c), (5 * i + 1) % 16)
        } else if i < 48 {
            (b ^ c ^ d, (3 * i + 5) % 16)
        } else {
            (c ^ (b | !d), (7 * i) % 16)
        };

        let temp = d;
        d = c;
        c = b;
        b = b.wrapping_add(left_rotate(a.wrapping_add(f).wrapping_add(K[i]).wrapping_add(block[g as usize]), R[i]));
        a = temp;
    }

    state[0] = state[0].wrapping_add(a);
    state[1] = state[1].wrapping_add(b);
    state[2] = state[2].wrapping_add(c);
    state[3] = state[3].wrapping_add(d);
}

fn compute_md5(msg: &[u8]) -> [u8; 16] {
    let mut h = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476];
    let len = msg.len();
    let num_blocks = ((len + 8) / 64) + 1;
    let padded_size = num_blocks * 64;
    let mut padded_msg = vec![0u8; padded_size];

    padded_msg[..len].copy_from_slice(msg);
    padded_msg[len] = 0x80;

    let bit_len = (len as u64) * 8;
    padded_msg[padded_size - 8..].copy_from_slice(&bit_len.to_le_bytes());

    for chunk in padded_msg.chunks_exact(64) {
        let mut block = [0u32; 16];
        for (i, word) in chunk.chunks_exact(4).enumerate() {
            block[i] = u32::from_le_bytes([word[0], word[1], word[2], word[3]]);
        }
        md5_transform(&mut h, &block);
    }

    let mut digest = [0u8; 16];
    for (i, &word) in h.iter().enumerate() {
        digest[i * 4..(i + 1) * 4].copy_from_slice(&word.to_le_bytes());
    }
    digest
}

fn find_advent_coin(secret_key: &str) -> i64 {
    let key_bytes = secret_key.as_bytes();
    let mut buffer = Vec::with_capacity(128);
    buffer.extend_from_slice(key_bytes);

    let mut number = 0;
    loop {
        buffer.truncate(key_bytes.len());
        let num_str = number.to_string();
        buffer.extend_from_slice(num_str.as_bytes());

        let digest = compute_md5(&buffer);
        if digest[0] == 0 && digest[1] == 0 && digest[2] == 0 {
            return number;
        }
        number += 1;
    }
}

fn main() {
    let mut file = File::open("input.txt").unwrap();
    let mut key = String::new();
    file.read_to_string(&mut key).unwrap();
    let key = key.trim_end();
    println!("{}", find_advent_coin(key));
}
