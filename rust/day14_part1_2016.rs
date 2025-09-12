
use std::fs;

type MD5u32 = u32;

fn f(x: MD5u32, y: MD5u32, z: MD5u32) -> MD5u32 {
    (x & y) | (!x & z)
}

fn g(x: MD5u32, y: MD5u32, z: MD5u32) -> MD5u32 {
    (x & z) | (y & !z)
}

fn h(x: MD5u32, y: MD5u32, z: MD5u32) -> MD5u32 {
    x ^ y ^ z
}

fn i(x: MD5u32, y: MD5u32, z: MD5u32) -> MD5u32 {
    y ^ (x | !z)
}

fn rotate_left(x: MD5u32, n: u32) -> MD5u32 {
    (x << n) | (x >> (32 - n))
}

fn md5_transform(state: &mut [MD5u32; 4], block: &[u8; 64]) {
    let mut a = state[0];
    let mut b = state[1];
    let mut c = state[2];
    let mut d = state[3];
    let mut x = [0u32; 16];

    for i in 0..16 {
        x[i] = (block[i * 4] as u32)
            | ((block[i * 4 + 1] as u32) << 8)
            | ((block[i * 4 + 2] as u32) << 16)
            | ((block[i * 4 + 3] as u32) << 24);
    }

    macro_rules! ff {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add(f($b, $c, $d).wrapping_add($x).wrapping_add($ac));
            $a = rotate_left($a, $s);
            $a = $a.wrapping_add($b);
        };
    }

    macro_rules! gg {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add(g($b, $c, $d).wrapping_add($x).wrapping_add($ac));
            $a = rotate_left($a, $s);
            $a = $a.wrapping_add($b);
        };
    }

    macro_rules! hh {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add(h($b, $c, $d).wrapping_add($x).wrapping_add($ac));
            $a = rotate_left($a, $s);
            $a = $a.wrapping_add($b);
        };
    }

    macro_rules! ii {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add(i($b, $c, $d).wrapping_add($x).wrapping_add($ac));
            $a = rotate_left($a, $s);
            $a = $a.wrapping_add($b);
        };
    }

    ff!(a, b, c, d, x[0], 7, 0xd76aa478);
    ff!(d, a, b, c, x[1], 12, 0xe8c7b756);
    ff!(c, d, a, b, x[2], 17, 0x242070db);
    ff!(b, c, d, a, x[3], 22, 0xc1bdceee);
    ff!(a, b, c, d, x[4], 7, 0xf57c0faf);
    ff!(d, a, b, c, x[5], 12, 0x4787c62a);
    ff!(c, d, a, b, x[6], 17, 0xa8304613);
    ff!(b, c, d, a, x[7], 22, 0xfd469501);
    ff!(a, b, c, d, x[8], 7, 0x698098d8);
    ff!(d, a, b, c, x[9], 12, 0x8b44f7af);
    ff!(c, d, a, b, x[10], 17, 0xffff5bb1);
    ff!(b, c, d, a, x[11], 22, 0x895cd7be);
    ff!(a, b, c, d, x[12], 7, 0x6b901122);
    ff!(d, a, b, c, x[13], 12, 0xfd987193);
    ff!(c, d, a, b, x[14], 17, 0xa679438e);
    ff!(b, c, d, a, x[15], 22, 0x49b40821);

    gg!(a, b, c, d, x[1], 5, 0xf61e2562);
    gg!(d, a, b, c, x[6], 9, 0xc040b340);
    gg!(c, d, a, b, x[11], 14, 0x265e5a51);
    gg!(b, c, d, a, x[0], 20, 0xe9b6c7aa);
    gg!(a, b, c, d, x[5], 5, 0xd62f105d);
    gg!(d, a, b, c, x[10], 9, 0x02441453);
    gg!(c, d, a, b, x[15], 14, 0xd8a1e681);
    gg!(b, c, d, a, x[4], 20, 0xe7d3fbc8);
    gg!(a, b, c, d, x[9], 5, 0x21e1cde6);
    gg!(d, a, b, c, x[14], 9, 0xc33707d6);
    gg!(c, d, a, b, x[3], 14, 0xf4d50d87);
    gg!(b, c, d, a, x[8], 20, 0x455a14ed);
    gg!(a, b, c, d, x[13], 5, 0xa9e3e905);
    gg!(d, a, b, c, x[2], 9, 0xfcefa3f8);
    gg!(c, d, a, b, x[7], 14, 0x676f02d9);
    gg!(b, c, d, a, x[12], 20, 0x8d2a4c8a);

    hh!(a, b, c, d, x[5], 4, 0xfffa3942);
    hh!(d, a, b, c, x[8], 11, 0x8771f681);
    hh!(c, d, a, b, x[11], 16, 0x6d9d6122);
    hh!(b, c, d, a, x[14], 23, 0xfde5380c);
    hh!(a, b, c, d, x[1], 4, 0xa4beea44);
    hh!(d, a, b, c, x[4], 11, 0x4bdecfa9);
    hh!(c, d, a, b, x[7], 16, 0xf6bb4b60);
    hh!(b, c, d, a, x[10], 23, 0xbebfbc70);
    hh!(a, b, c, d, x[13], 4, 0x289b7ec6);
    hh!(d, a, b, c, x[0], 11, 0xeaa127fa);
    hh!(c, d, a, b, x[3], 16, 0xd4ef3085);
    hh!(b, c, d, a, x[6], 23, 0x04881d05);
    hh!(a, b, c, d, x[9], 4, 0xd9d4d039);
    hh!(d, a, b, c, x[12], 11, 0xe6db99e5);
    hh!(c, d, a, b, x[15], 16, 0x1fa27cf8);
    hh!(b, c, d, a, x[2], 23, 0xc4ac5665);

    ii!(a, b, c, d, x[0], 6, 0xf4292244);
    ii!(d, a, b, c, x[7], 10, 0x432aff97);
    ii!(c, d, a, b, x[14], 15, 0xab9423a7);
    ii!(b, c, d, a, x[5], 21, 0xfc93a039);
    ii!(a, b, c, d, x[12], 6, 0x655b59c3);
    ii!(d, a, b, c, x[3], 10, 0x8f0ccc92);
    ii!(c, d, a, b, x[10], 15, 0xffeff47d);
    ii!(b, c, d, a, x[1], 21, 0x85845dd1);
    ii!(a, b, c, d, x[8], 6, 0x6fa87e4f);
    ii!(d, a, b, c, x[15], 10, 0xfe2ce6e0);
    ii!(c, d, a, b, x[6], 15, 0xa3014314);
    ii!(b, c, d, a, x[13], 21, 0x4e0811a1);
    ii!(a, b, c, d, x[4], 6, 0xf7537e82);
    ii!(d, a, b, c, x[11], 10, 0xbd3af235);
    ii!(c, d, a, b, x[2], 15, 0x2ad7d2bb);
    ii!(b, c, d, a, x[9], 21, 0xeb86d391);

    state[0] = state[0].wrapping_add(a);
    state[1] = state[1].wrapping_add(b);
    state[2] = state[2].wrapping_add(c);
    state[3] = state[3].wrapping_add(d);
}

fn md5(message: &str) -> String {
    let mut state: [MD5u32; 4] = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476];
    let original_len = message.len();
    let padded_len = original_len + 8;
    let padded_len = padded_len + (64 - (padded_len % 64)) % 64;
    let mut padded_message = vec![0u8; padded_len];

    padded_message[..original_len].copy_from_slice(message.as_bytes());
    padded_message[original_len] = 0x80;

    let bits_len = (original_len * 8) as u64;
    for i in 0..8 {
        padded_message[padded_len - 8 + i] = ((bits_len >> (i * 8)) & 0xFF) as u8;
    }

    for chunk in padded_message.chunks_exact(64) {
        let mut block = [0u8; 64];
        block.copy_from_slice(chunk);
        md5_transform(&mut state, &block);
    }

    let mut result = String::with_capacity(32);
    for &word in &state {
        result.push_str(&format!("{:02x}", (word & 0xFF) as u8));
        result.push_str(&format!("{:02x}", ((word >> 8) & 0xFF) as u8));
        result.push_str(&format!("{:02x}", ((word >> 16) & 0xFF) as u8));
        result.push_str(&format!("{:02x}", ((word >> 24) & 0xFF) as u8));
    }

    result
}

fn find_triplet(hash_str: &str) -> Option<char> {
    hash_str
        .as_bytes()
        .windows(3)
        .find(|w| w[0] == w[1] && w[1] == w[2])
        .map(|w| w[0] as char)
}

fn has_quintuplet(hash_str: &str, c: char) -> bool {
    let quint = [c; 5].iter().collect::<String>();
    hash_str.contains(&quint)
}

fn main() {
    let salt = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut cached_hashes = Vec::new();
    let mut keys = 0;
    let mut index = 0;

    while keys < 64 {
        let hash = if index < cached_hashes.len() {
            &cached_hashes[index]
        } else {
            let new_hash = md5(&format!("{}{}", salt, index));
            cached_hashes.push(new_hash);
            &cached_hashes[index]
        };

        if let Some(triplet_char) = find_triplet(hash) {
            for i in 1..=1000 {
                let check_index = index + i;
                let check_hash = if check_index < cached_hashes.len() {
                    &cached_hashes[check_index]
                } else {
                    let new_hash = md5(&format!("{}{}", salt, check_index));
                    cached_hashes.push(new_hash);
                    &cached_hashes[check_index]
                };

                if has_quintuplet(check_hash, triplet_char) {
                    keys += 1;
                    break;
                }
            }
        }
        index += 1;
    }

    println!("{}", index - 1);
}
