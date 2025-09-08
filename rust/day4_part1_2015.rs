
use std::fs;

const K: [u32; 64] = [
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a,
    0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340,
    0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8,
    0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
    0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92,
    0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
];

const R: [u32; 64] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
];

fn compute_md5(msg_in: &[u8], hash_out: &mut [u32; 4]) {
    hash_out[0] = 0x67452301;
    hash_out[1] = 0xEFCDAB89;
    hash_out[2] = 0x98BADCFE;
    hash_out[3] = 0x10325476;

    let len_in = msg_in.len();
    let padded_len = (((len_in + 8) / 64) + 1) * 64;
    let mut msg = vec![0u8; padded_len];
    msg[..len_in].copy_from_slice(msg_in);
    msg[len_in] = 0x80;
    let bit_len = (len_in * 8) as u64;
    msg[padded_len - 8..].copy_from_slice(&bit_len.to_le_bytes());

    for offset in (0..padded_len).step_by(64) {
        let mut w = [0u32; 16];
        for i in 0..16 {
            w[i] = u32::from_le_bytes([
                msg[offset + i * 4],
                msg[offset + i * 4 + 1],
                msg[offset + i * 4 + 2],
                msg[offset + i * 4 + 3],
            ]);
        }

        let mut a = hash_out[0];
        let mut b = hash_out[1];
        let mut c = hash_out[2];
        let mut d = hash_out[3];

        for i in 0..64 {
            let (f, g) = match i {
                0..=15 => ((b & c) | (!b & d), i),
                16..=31 => ((d & b) | (!d & c), (5 * i + 1) % 16),
                32..=47 => (b ^ c ^ d, (3 * i + 5) % 16),
                _ => (c ^ (b | !d), (7 * i) % 16),
            };

            let temp = d;
            d = c;
            c = b;
            b = b.wrapping_add((a.wrapping_add(f).wrapping_add(K[i]).wrapping_add(w[g as usize])).rotate_left(R[i]));
            a = temp;
        }

        hash_out[0] = hash_out[0].wrapping_add(a);
        hash_out[1] = hash_out[1].wrapping_add(b);
        hash_out[2] = hash_out[2].wrapping_add(c);
        hash_out[3] = hash_out[3].wrapping_add(d);
    }
}

fn main() {
    let key = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut number = 0;
    let mut buffer = String::new();
    let mut hash = [0u32; 4];

    loop {
        buffer.clear();
        buffer.push_str(&key);
        buffer.push_str(&number.to_string());
        compute_md5(buffer.as_bytes(), &mut hash);
        if hash[0] & 0x00F0FFFF == 0 {
            println!("{}", number);
            break;
        }
        number += 1;
    }
}
