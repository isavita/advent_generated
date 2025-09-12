
use std::collections::VecDeque;
use std::fs;

fn rotate_left(x: u32, n: u32) -> u32 {
    x.rotate_left(n)
}

fn f(x: u32, y: u32, z: u32) -> u32 {
    (x & y) | (!x & z)
}

fn g(x: u32, y: u32, z: u32) -> u32 {
    (x & z) | (y & !z)
}

fn h(x: u32, y: u32, z: u32) -> u32 {
    x ^ y ^ z
}

fn i(x: u32, y: u32, z: u32) -> u32 {
    y ^ (x | !z)
}

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

const S: [u32; 64] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9,
    14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
];

struct Md5Ctx {
    state: [u32; 4],
    count: [u32; 2],
    buffer: [u8; 64],
}

impl Md5Ctx {
    fn new() -> Self {
        Self {
            state: [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476],
            count: [0, 0],
            buffer: [0; 64],
        }
    }
}

fn md5_transform(state: &mut [u32; 4], block: &[u8]) {
    let mut a = state[0];
    let mut b = state[1];
    let mut c = state[2];
    let mut d = state[3];
    let mut x = [0u32; 16];

    for i in 0..16 {
        x[i] = u32::from(block[i * 4])
            | (u32::from(block[i * 4 + 1]) << 8)
            | (u32::from(block[i * 4 + 2]) << 16)
            | (u32::from(block[i * 4 + 3]) << 24);
    }

    macro_rules! round {
        ($f:ident, $a:ident, $b:ident, $c:ident, $d:ident, $x:expr, $s:expr, $k:expr) => {
            $a = $a.wrapping_add($f($b, $c, $d).wrapping_add($x).wrapping_add($k));
            $a = rotate_left($a, $s).wrapping_add($b);
        };
    }

    round!(f, a, b, c, d, x[0], S[0], K[0]);
    round!(f, d, a, b, c, x[1], S[1], K[1]);
    round!(f, c, d, a, b, x[2], S[2], K[2]);
    round!(f, b, c, d, a, x[3], S[3], K[3]);
    round!(f, a, b, c, d, x[4], S[4], K[4]);
    round!(f, d, a, b, c, x[5], S[5], K[5]);
    round!(f, c, d, a, b, x[6], S[6], K[6]);
    round!(f, b, c, d, a, x[7], S[7], K[7]);
    round!(f, a, b, c, d, x[8], S[8], K[8]);
    round!(f, d, a, b, c, x[9], S[9], K[9]);
    round!(f, c, d, a, b, x[10], S[10], K[10]);
    round!(f, b, c, d, a, x[11], S[11], K[11]);
    round!(f, a, b, c, d, x[12], S[12], K[12]);
    round!(f, d, a, b, c, x[13], S[13], K[13]);
    round!(f, c, d, a, b, x[14], S[14], K[14]);
    round!(f, b, c, d, a, x[15], S[15], K[15]);

    round!(g, a, b, c, d, x[1], S[16], K[16]);
    round!(g, d, a, b, c, x[6], S[17], K[17]);
    round!(g, c, d, a, b, x[11], S[18], K[18]);
    round!(g, b, c, d, a, x[0], S[19], K[19]);
    round!(g, a, b, c, d, x[5], S[20], K[20]);
    round!(g, d, a, b, c, x[10], S[21], K[21]);
    round!(g, c, d, a, b, x[15], S[22], K[22]);
    round!(g, b, c, d, a, x[4], S[23], K[23]);
    round!(g, a, b, c, d, x[9], S[24], K[24]);
    round!(g, d, a, b, c, x[14], S[25], K[25]);
    round!(g, c, d, a, b, x[3], S[26], K[26]);
    round!(g, b, c, d, a, x[8], S[27], K[27]);
    round!(g, a, b, c, d, x[13], S[28], K[28]);
    round!(g, d, a, b, c, x[2], S[29], K[29]);
    round!(g, c, d, a, b, x[7], S[30], K[30]);
    round!(g, b, c, d, a, x[12], S[31], K[31]);

    round!(h, a, b, c, d, x[5], S[32], K[32]);
    round!(h, d, a, b, c, x[8], S[33], K[33]);
    round!(h, c, d, a, b, x[11], S[34], K[34]);
    round!(h, b, c, d, a, x[14], S[35], K[35]);
    round!(h, a, b, c, d, x[1], S[36], K[36]);
    round!(h, d, a, b, c, x[4], S[37], K[37]);
    round!(h, c, d, a, b, x[7], S[38], K[38]);
    round!(h, b, c, d, a, x[10], S[39], K[39]);
    round!(h, a, b, c, d, x[13], S[40], K[40]);
    round!(h, d, a, b, c, x[0], S[41], K[41]);
    round!(h, c, d, a, b, x[3], S[42], K[42]);
    round!(h, b, c, d, a, x[6], S[43], K[43]);
    round!(h, a, b, c, d, x[9], S[44], K[44]);
    round!(h, d, a, b, c, x[12], S[45], K[45]);
    round!(h, c, d, a, b, x[15], S[46], K[46]);
    round!(h, b, c, d, a, x[2], S[47], K[47]);

    round!(i, a, b, c, d, x[0], S[48], K[48]);
    round!(i, d, a, b, c, x[7], S[49], K[49]);
    round!(i, c, d, a, b, x[14], S[50], K[50]);
    round!(i, b, c, d, a, x[5], S[51], K[51]);
    round!(i, a, b, c, d, x[12], S[52], K[52]);
    round!(i, d, a, b, c, x[3], S[53], K[53]);
    round!(i, c, d, a, b, x[10], S[54], K[54]);
    round!(i, b, c, d, a, x[1], S[55], K[55]);
    round!(i, a, b, c, d, x[8], S[56], K[56]);
    round!(i, d, a, b, c, x[15], S[57], K[57]);
    round!(i, c, d, a, b, x[6], S[58], K[58]);
    round!(i, b, c, d, a, x[13], S[59], K[59]);
    round!(i, a, b, c, d, x[4], S[60], K[60]);
    round!(i, d, a, b, c, x[11], S[61], K[61]);
    round!(i, c, d, a, b, x[2], S[62], K[62]);
    round!(i, b, c, d, a, x[9], S[63], K[63]);

    state[0] = state[0].wrapping_add(a);
    state[1] = state[1].wrapping_add(b);
    state[2] = state[2].wrapping_add(c);
    state[3] = state[3].wrapping_add(d);
}

fn md5_update(ctx: &mut Md5Ctx, input: &[u8]) {
    let mut idx = ((ctx.count[0] >> 3) & 0x3F) as usize;
    let input_len = input.len() as u32;

    ctx.count[0] = ctx.count[0].wrapping_add(input_len << 3);
    if ctx.count[0] < input_len << 3 {
        ctx.count[1] += 1;
    }
    ctx.count[1] += input_len >> 29;

    let part_len = 64 - idx;
    let mut i = 0;

    if input_len >= part_len as u32 {
        ctx.buffer[idx..idx + part_len].copy_from_slice(&input[..part_len]);
        md5_transform(&mut ctx.state, &ctx.buffer);
        i = part_len;
        while i + 64 <= input.len() {
            md5_transform(&mut ctx.state, &input[i..i + 64]);
            i += 64;
        }
        idx = 0;
    }

    if i < input.len() {
        ctx.buffer[idx..idx + input.len() - i].copy_from_slice(&input[i..]);
    }
}

fn md5_final(ctx: &mut Md5Ctx) -> [u8; 16] {
    let mut bits = [0u8; 8];
    bits[0] = ctx.count[0] as u8;
    bits[1] = (ctx.count[0] >> 8) as u8;
    bits[2] = (ctx.count[0] >> 16) as u8;
    bits[3] = (ctx.count[0] >> 24) as u8;
    bits[4] = ctx.count[1] as u8;
    bits[5] = (ctx.count[1] >> 8) as u8;
    bits[6] = (ctx.count[1] >> 16) as u8;
    bits[7] = (ctx.count[1] >> 24) as u8;

    let idx = ((ctx.count[0] >> 3) & 0x3F) as usize;
    let pad_len = if idx < 56 { 56 - idx } else { 120 - idx };
    let mut padding = vec![0x80];
    padding.resize(pad_len, 0);

    md5_update(ctx, &padding);
    md5_update(ctx, &bits);

    let mut digest = [0u8; 16];
    for j in 0..4 {
        digest[j * 4] = ctx.state[j] as u8;
        digest[j * 4 + 1] = (ctx.state[j] >> 8) as u8;
        digest[j * 4 + 2] = (ctx.state[j] >> 16) as u8;
        digest[j * 4 + 3] = (ctx.state[j] >> 24) as u8;
    }

    digest
}

fn calculate_md5(input: &str) -> String {
    let mut ctx = Md5Ctx::new();
    md5_update(&mut ctx, input.as_bytes());
    let digest = md5_final(&mut ctx);
    digest.iter().map(|b| format!("{:02x}", b)).collect()
}

fn is_open(c: char) -> bool {
    c >= 'b' && c <= 'f'
}

fn get_open_doors(passcode: &str, path: &str) -> Vec<char> {
    let hash_input = format!("{}{}", passcode, path);
    let hash = calculate_md5(&hash_input);
    let mut doors = Vec::new();
    let chars: Vec<char> = hash.chars().collect();

    if is_open(chars[0]) {
        doors.push('U');
    }
    if is_open(chars[1]) {
        doors.push('D');
    }
    if is_open(chars[2]) {
        doors.push('L');
    }
    if is_open(chars[3]) {
        doors.push('R');
    }
    doors
}

fn find_longest_path_length(passcode: &str) -> usize {
    let mut longest = 0;
    let mut queue = VecDeque::new();
    queue.push_back((0, 0, String::new()));

    while let Some((x, y, path)) = queue.pop_front() {
        if x == 3 && y == 3 {
            longest = longest.max(path.len());
            continue;
        }

        let open_doors = get_open_doors(passcode, &path);
        for dir in open_doors {
            let mut next_path = path.clone();
            next_path.push(dir);
            let (nx, ny) = match dir {
                'U' => (x, y - 1),
                'D' => (x, y + 1),
                'L' => (x - 1, y),
                'R' => (x + 1, y),
                _ => continue,
            };

            if nx >= 0 && nx < 4 && ny >= 0 && ny < 4 {
                queue.push_back((nx, ny, next_path));
            }
        }
    }
    longest
}

fn main() {
    let passcode = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let longest_path_length = find_longest_path_length(&passcode);
    println!("{}", longest_path_length);
}
