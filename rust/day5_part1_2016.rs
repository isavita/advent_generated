
use std::fs;

const PADDING: [u8; 64] = [
    0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
];

struct Md5Ctx {
    state: [u32; 4],
    count: [u32; 2],
    buffer: [u8; 64],
}

fn f(x: u32, y: u32, z: u32) -> u32 { (x & y) | (!x & z) }
fn g(x: u32, y: u32, z: u32) -> u32 { (x & z) | (y & !z) }
fn h(x: u32, y: u32, z: u32) -> u32 { x ^ y ^ z }
fn i(x: u32, y: u32, z: u32) -> u32 { y ^ (x | !z) }

fn rotate_left(x: u32, n: u32) -> u32 { (x << n) | (x >> (32 - n)) }

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

fn md5_init(ctx: &mut Md5Ctx) {
    ctx.count[0] = 0;
    ctx.count[1] = 0;
    ctx.state[0] = 0x67452301;
    ctx.state[1] = 0xEFCDAB89;
    ctx.state[2] = 0x98BADCFE;
    ctx.state[3] = 0x10325476;
}

fn encode(output: &mut [u8], input: &[u32]) {
    for (i, &val) in input.iter().enumerate() {
        output[i * 4] = (val & 0xFF) as u8;
        output[i * 4 + 1] = ((val >> 8) & 0xFF) as u8;
        output[i * 4 + 2] = ((val >> 16) & 0xFF) as u8;
        output[i * 4 + 3] = ((val >> 24) & 0xFF) as u8;
    }
}

fn decode(output: &mut [u32], input: &[u8]) {
    for i in 0..output.len() {
        output[i] = (input[i * 4] as u32)
            | ((input[i * 4 + 1] as u32) << 8)
            | ((input[i * 4 + 2] as u32) << 16)
            | ((input[i * 4 + 3] as u32) << 24);
    }
}

fn md5_transform(state: &mut [u32; 4], block: &[u8; 64]) {
    let mut a = state[0];
    let mut b = state[1];
    let mut c = state[2];
    let mut d = state[3];
    let mut x = [0u32; 16];

    decode(&mut x, block);

    ff!(a, b, c, d, x[0], 7, 0xd76aa478); ff!(d, a, b, c, x[1], 12, 0xe8c7b756);
    ff!(c, d, a, b, x[2], 17, 0x242070db); ff!(b, c, d, a, x[3], 22, 0xc1bdceee);
    ff!(a, b, c, d, x[4], 7, 0xf57c0faf); ff!(d, a, b, c, x[5], 12, 0x4787c62a);
    ff!(c, d, a, b, x[6], 17, 0xa8304613); ff!(b, c, d, a, x[7], 22, 0xfd469501);
    ff!(a, b, c, d, x[8], 7, 0x698098d8); ff!(d, a, b, c, x[9], 12, 0x8b44f7af);
    ff!(c, d, a, b, x[10], 17, 0xffff5bb1); ff!(b, c, d, a, x[11], 22, 0x895cd7be);
    ff!(a, b, c, d, x[12], 7, 0x6b901122); ff!(d, a, b, c, x[13], 12, 0xfd987193);
    ff!(c, d, a, b, x[14], 17, 0xa679438e); ff!(b, c, d, a, x[15], 22, 0x49b40821);

    gg!(a, b, c, d, x[1], 5, 0xf61e2562); gg!(d, a, b, c, x[6], 9, 0xc040b340);
    gg!(c, d, a, b, x[11], 14, 0x265e5a51); gg!(b, c, d, a, x[0], 20, 0xe9b6c7aa);
    gg!(a, b, c, d, x[5], 5, 0xd62f105d); gg!(d, a, b, c, x[10], 9, 0x02441453);
    gg!(c, d, a, b, x[15], 14, 0xd8a1e681); gg!(b, c, d, a, x[4], 20, 0xe7d3fbc8);
    gg!(a, b, c, d, x[9], 5, 0x21e1cde6); gg!(d, a, b, c, x[14], 9, 0xc33707d6);
    gg!(c, d, a, b, x[3], 14, 0xf4d50d87); gg!(b, c, d, a, x[8], 20, 0x455a14ed);
    gg!(a, b, c, d, x[13], 5, 0xa9e3e905); gg!(d, a, b, c, x[2], 9, 0xfcefa3f8);
    gg!(c, d, a, b, x[7], 14, 0x676f02d9); gg!(b, c, d, a, x[12], 20, 0x8d2a4c8a);

    hh!(a, b, c, d, x[5], 4, 0xfffa3942); hh!(d, a, b, c, x[8], 11, 0x8771f681);
    hh!(c, d, a, b, x[11], 16, 0x6d9d6122); hh!(b, c, d, a, x[14], 23, 0xfde5380c);
    hh!(a, b, c, d, x[1], 4, 0xa4beea44); hh!(d, a, b, c, x[4], 11, 0x4bdecfa9);
    hh!(c, d, a, b, x[7], 16, 0xf6bb4b60); hh!(b, c, d, a, x[10], 23, 0xbebfbc70);
    hh!(a, b, c, d, x[13], 4, 0x289b7ec6); hh!(d, a, b, c, x[0], 11, 0xeaa127fa);
    hh!(c, d, a, b, x[3], 16, 0xd4ef3085); hh!(b, c, d, a, x[6], 23, 0x04881d05);
    hh!(a, b, c, d, x[9], 4, 0xd9d4d039); hh!(d, a, b, c, x[12], 11, 0xe6db99e5);
    hh!(c, d, a, b, x[15], 16, 0x1fa27cf8); hh!(b, c, d, a, x[2], 23, 0xc4ac5665);

    ii!(a, b, c, d, x[0], 6, 0xf4292244); ii!(d, a, b, c, x[7], 10, 0x432aff97);
    ii!(c, d, a, b, x[14], 15, 0xab9423a7); ii!(b, c, d, a, x[5], 21, 0xfc93a039);
    ii!(a, b, c, d, x[12], 6, 0x655b59c3); ii!(d, a, b, c, x[3], 10, 0x8f0ccc92);
    ii!(c, d, a, b, x[10], 15, 0xffeff47d); ii!(b, c, d, a, x[1], 21, 0x85845dd1);
    ii!(a, b, c, d, x[8], 6, 0x6fa87e4f); ii!(d, a, b, c, x[15], 10, 0xfe2ce6e0);
    ii!(c, d, a, b, x[6], 15, 0xa3014314); ii!(b, c, d, a, x[13], 21, 0x4e0811a1);
    ii!(a, b, c, d, x[4], 6, 0xf7537e82); ii!(d, a, b, c, x[11], 10, 0xbd3af235);
    ii!(c, d, a, b, x[2], 15, 0x2ad7d2bb); ii!(b, c, d, a, x[9], 21, 0xeb86d391);

    state[0] = state[0].wrapping_add(a);
    state[1] = state[1].wrapping_add(b);
    state[2] = state[2].wrapping_add(c);
    state[3] = state[3].wrapping_add(d);
}

fn md5_update(ctx: &mut Md5Ctx, input: &[u8]) {
    let mut index = ((ctx.count[0] >> 3) & 0x3F) as usize;
    let input_len = input.len() as u32;

    ctx.count[0] = ctx.count[0].wrapping_add(input_len << 3);
    if ctx.count[0] < input_len << 3 {
        ctx.count[1] = ctx.count[1].wrapping_add(1);
    }
    ctx.count[1] = ctx.count[1].wrapping_add(input_len >> 29);

    let part_len = 64 - index;

    if input_len as usize >= part_len {
        ctx.buffer[index..index + part_len].copy_from_slice(&input[0..part_len]);
        md5_transform(&mut ctx.state, &ctx.buffer);

        let mut i = part_len;
        while i + 64 <= input.len() {
            let mut block = [0u8; 64];
            block.copy_from_slice(&input[i..i + 64]);
            md5_transform(&mut ctx.state, &block);
            i += 64;
        }
        index = 0;
        ctx.buffer[..input.len() - i].copy_from_slice(&input[i..]);
    } else {
        ctx.buffer[index..index + input.len()].copy_from_slice(input);
    }
}

fn md5_final(ctx: &mut Md5Ctx) -> [u8; 16] {
    let mut bits = [0u8; 8];
    encode(&mut bits, &ctx.count);

    let index = ((ctx.count[0] >> 3) & 0x3F) as usize;
    let pad_len = if index < 56 { 56 - index } else { 120 - index };

    md5_update(ctx, &PADDING[..pad_len]);
    md5_update(ctx, &bits);

    let mut digest = [0u8; 16];
    encode(&mut digest, &ctx.state);
    digest
}

fn md5_hexdigest(input: &str) -> String {
    let mut ctx = Md5Ctx {
        state: [0; 4],
        count: [0; 2],
        buffer: [0; 64],
    };
    md5_init(&mut ctx);
    md5_update(&mut ctx, input.as_bytes());
    let digest = md5_final(&mut ctx);
    digest.iter().map(|b| format!("{:02x}", b)).collect()
}

fn main() {
    let door_id = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut password = String::new();
    let mut index = 0;

    while password.len() < 8 {
        let hash_input = format!("{}{}", door_id, index);
        let result = md5_hexdigest(&hash_input);
        
        if result.starts_with("00000") {
            password.push(result.chars().nth(5).unwrap());
        }
        
        index += 1;
    }

    println!("{}", password);
}
