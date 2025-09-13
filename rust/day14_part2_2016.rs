use std::fs::File;
use std::io::Read;

const S: [u32; 64] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
];

const K: [u32; 64] = [
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
];

#[inline]
fn rol(x: u32, c: u32) -> u32 {
    (x << c) | (x >> (32 - c))
}

fn md5(input: &[u8]) -> [u8; 16] {
    let mut h0: u32 = 0x67452301;
    let mut h1: u32 = 0xEFCDAB89;
    let mut h2: u32 = 0x98BADCFE;
    let mut h3: u32 = 0x10325476;

    let mut data = input.to_vec();
    let bit_len = (data.len() as u64) * 8;

    data.push(0x80);
    while (data.len() % 64) != 56 {
        data.push(0);
    }
    data.extend_from_slice(&bit_len.to_le_bytes());

    for chunk in data.chunks(64) {
        let mut M = [0u32; 16];
        for i in 0..16 {
            let j = i * 4;
            M[i] = u32::from_le_bytes([
                chunk[j],
                chunk[j + 1],
                chunk[j + 2],
                chunk[j + 3],
            ]);
        }

        let mut A = h0;
        let mut B = h1;
        let mut C = h2;
        let mut D = h3;

        for j in 0..64 {
            let (F, g) = if j < 16 {
                let f = (B & C) | ((!B) & D);
                (f, j)
            } else if j < 32 {
                let f = (D & B) | ((!D) & C);
                (f, (5 * j + 1) % 16)
            } else if j < 48 {
                let f = B ^ C ^ D;
                (f, (3 * j + 5) % 16)
            } else {
                let f = C ^ (B | !D);
                (f, (7 * j) % 16)
            };

            let tmp = D;
            D = C;
            C = B;
            let sum = A
                .wrapping_add(F)
                .wrapping_add(K[j])
                .wrapping_add(M[g]);
            B = B.wrapping_add(rol(sum, S[j]));
            A = tmp;
        }

        h0 = h0.wrapping_add(A);
        h1 = h1.wrapping_add(B);
        h2 = h2.wrapping_add(C);
        h3 = h3.wrapping_add(D);
    }

    let mut digest = [0u8; 16];
    digest[0..4].copy_from_slice(&h0.to_le_bytes());
    digest[4..8].copy_from_slice(&h1.to_le_bytes());
    digest[8..12].copy_from_slice(&h2.to_le_bytes());
    digest[12..16].copy_from_slice(&h3.to_le_bytes());
    digest
}

fn to_hex(d: &[u8; 16]) -> String {
    let mut s = String::with_capacity(32);
    for &b in d.iter() {
        s.push_str(&format!("{:02x}", b));
    }
    s
}

const CACHE_SIZE: usize = 40000;

fn get_stretched_hash(salt: &str, idx: usize, cache: &mut Vec<Option<String>>) -> String {
    if idx >= CACHE_SIZE {
        panic!("index out of cache bounds");
    }
    if let Some(ref v) = cache[idx] {
        return v.clone();
    }

    let mut msg = String::with_capacity(salt.len() + 10);
    msg.push_str(salt);
    msg.push_str(&idx.to_string());

    let mut digest = md5(msg.as_bytes());
    let mut hex_str = to_hex(&digest);

    for _ in 0..2016 {
        digest = md5(hex_str.as_bytes());
        hex_str = to_hex(&digest);
    }

    cache[idx] = Some(hex_str.clone());
    hex_str
}

fn main() {
    let mut f = File::open("input.txt").unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    let salt = contents.lines().next().unwrap_or("").trim();

    let mut cache: Vec<Option<String>> = vec![None; CACHE_SIZE];
    let mut found = 0;
    let mut index: usize = 0;

    loop {
        let hash = get_stretched_hash(salt, index, &mut cache);
        let bytes = hash.as_bytes();
        let mut triplet_b: Option<u8> = None;
        for i in 0..30 {
            if bytes[i] == bytes[i + 1] && bytes[i] == bytes[i + 2] {
                triplet_b = Some(bytes[i]);
                break;
            }
        }

        if let Some(b) = triplet_b {
            'inner: for j in 1..=1000 {
                let h_next = get_stretched_hash(salt, index + j, &mut cache);
                let nb = h_next.as_bytes();
                for t in 0..=(nb.len() - 5) {
                    if nb[t] == b && nb[t + 1] == b && nb[t + 2] == b && nb[t + 3] == b && nb[t + 4] == b {
                        found += 1;
                        break 'inner;
                    }
                }
            }
        }

        if found == 64 {
            println!("{}", index);
            break;
        }
        index += 1;
    }
}