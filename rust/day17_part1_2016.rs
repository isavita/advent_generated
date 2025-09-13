
use std::collections::VecDeque;
use std::fs;

struct MD5Context {
    state: [u32; 4],
    count: [u32; 2],
    buffer: [u8; 64],
}

const F: fn(u32, u32, u32) -> u32 = |x, y, z| (x & y) | (!x & z);
const G: fn(u32, u32, u32) -> u32 = |x, y, z| (x & z) | (y & !z);
const H: fn(u32, u32, u32) -> u32 = |x, y, z| x ^ y ^ z;
const I: fn(u32, u32, u32) -> u32 = |x, y, z| y ^ (x | !z);

fn rotate_left(x: u32, n: u32) -> u32 {
    (x << n) | (x >> (32 - n))
}

fn md5_transform(state: &mut [u32; 4], block: &[u8; 64]) {
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

    macro_rules! op {
        ($func:ident, $a:ident, $b:ident, $c:ident, $d:ident, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add($func($b, $c, $d).wrapping_add($x).wrapping_add($ac));
            $a = rotate_left($a, $s);
            $a = $a.wrapping_add($b);
        };
    }

    op!(F, a, b, c, d, x[0], 7, 0xd76aa478);
    op!(F, d, a, b, c, x[1], 12, 0xe8c7b756);
    op!(F, c, d, a, b, x[2], 17, 0x242070db);
    op!(F, b, c, d, a, x[3], 22, 0xc1bdceee);
    op!(F, a, b, c, d, x[4], 7, 0xf57c0faf);
    op!(F, d, a, b, c, x[5], 12, 0x4787c62a);
    op!(F, c, d, a, b, x[6], 17, 0xa8304613);
    op!(F, b, c, d, a, x[7], 22, 0xfd469501);
    op!(F, a, b, c, d, x[8], 7, 0x698098d8);
    op!(F, d, a, b, c, x[9], 12, 0x8b44f7af);
    op!(F, c, d, a, b, x[10], 17, 0xffff5bb1);
    op!(F, b, c, d, a, x[11], 22, 0x895cd7be);
    op!(F, a, b, c, d, x[12], 7, 0x6b901122);
    op!(F, d, a, b, c, x[13], 12, 0xfd987193);
    op!(F, c, d, a, b, x[14], 17, 0xa679438e);
    op!(F, b, c, d, a, x[15], 22, 0x49b40821);

    op!(G, a, b, c, d, x[1], 5, 0xf61e2562);
    op!(G, d, a, b, c, x[6], 9, 0xc040b340);
    op!(G, c, d, a, b, x[11], 14, 0x265e5a51);
    op!(G, b, c, d, a, x[0], 20, 0xe9b6c7aa);
    op!(G, a, b, c, d, x[5], 5, 0xd62f105d);
    op!(G, d, a, b, c, x[10], 9, 0x02441453);
    op!(G, c, d, a, b, x[15], 14, 0xd8a1e681);
    op!(G, b, c, d, a, x[4], 20, 0xe7d3fbc8);
    op!(G, a, b, c, d, x[9], 5, 0x21e1cde6);
    op!(G, d, a, b, c, x[14], 9, 0xc33707d6);
    op!(G, c, d, a, b, x[3], 14, 0xf4d50d87);
    op!(G, b, c, d, a, x[8], 20, 0x455a14ed);
    op!(G, a, b, c, d, x[13], 5, 0xa9e3e905);
    op!(G, d, a, b, c, x[2], 9, 0xfcefa3f8);
    op!(G, c, d, a, b, x[7], 14, 0x676f02d9);
    op!(G, b, c, d, a, x[12], 20, 0x8d2a4c8a);

    op!(H, a, b, c, d, x[5], 4, 0xfffa3942);
    op!(H, d, a, b, c, x[8], 11, 0x8771f681);
    op!(H, c, d, a, b, x[11], 16, 0x6d9d6122);
    op!(H, b, c, d, a, x[14], 23, 0xfde5380c);
    op!(H, a, b, c, d, x[1], 4, 0xa4beea44);
    op!(H, d, a, b, c, x[4], 11, 0x4bdecfa9);
    op!(H, c, d, a, b, x[7], 16, 0xf6bb4b60);
    op!(H, b, c, d, a, x[10], 23, 0xbebfbc70);
    op!(H, a, b, c, d, x[13], 4, 0x289b7ec6);
    op!(H, d, a, b, c, x[0], 11, 0xeaa127fa);
    op!(H, c, d, a, b, x[3], 16, 0xd4ef3085);
    op!(H, b, c, d, a, x[6], 23, 0x04881d05);
    op!(H, a, b, c, d, x[9], 4, 0xd9d4d039);
    op!(H, d, a, b, c, x[12], 11, 0xe6db99e5);
    op!(H, c, d, a, b, x[15], 16, 0x1fa27cf8);
    op!(H, b, c, d, a, x[2], 23, 0xc4ac5665);

    op!(I, a, b, c, d, x[0], 6, 0xf4292244);
    op!(I, d, a, b, c, x[7], 10, 0x432aff97);
    op!(I, c, d, a, b, x[14], 15, 0xab9423a7);
    op!(I, b, c, d, a, x[5], 21, 0xfc93a039);
    op!(I, a, b, c, d, x[12], 6, 0x655b59c3);
    op!(I, d, a, b, c, x[3], 10, 0x8f0ccc92);
    op!(I, c, d, a, b, x[10], 15, 0xffeff47d);
    op!(I, b, c, d, a, x[1], 21, 0x85845dd1);
    op!(I, a, b, c, d, x[8], 6, 0x6fa87e4f);
    op!(I, d, a, b, c, x[15], 10, 0xfe2ce6e0);
    op!(I, c, d, a, b, x[6], 15, 0xa3014314);
    op!(I, b, c, d, a, x[13], 21, 0x4e0811a1);
    op!(I, a, b, c, d, x[4], 6, 0xf7537e82);
    op!(I, d, a, b, c, x[11], 10, 0xbd3af235);
    op!(I, c, d, a, b, x[2], 15, 0x2ad7d2bb);
    op!(I, b, c, d, a, x[9], 21, 0xeb86d391);

    state[0] = state[0].wrapping_add(a);
    state[1] = state[1].wrapping_add(b);
    state[2] = state[2].wrapping_add(c);
    state[3] = state[3].wrapping_add(d);
}

fn md5_init(context: &mut MD5Context) {
    context.count = [0, 0];
    context.state = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476];
}

fn md5_update(context: &mut MD5Context, input: &[u8]) {
    let input_len = input.len() as u32;
    let mut index = ((context.count[0] >> 3) & 0x3F) as usize;
    
    context.count[0] = context.count[0].wrapping_add(input_len << 3);
    if context.count[0] < input_len << 3 {
        context.count[1] = context.count[1].wrapping_add(1);
    }
    context.count[1] = context.count[1].wrapping_add(input_len >> 29);
    
    let part_len = 64 - index;
    
    if input_len as usize >= part_len {
        context.buffer[index..index + part_len].copy_from_slice(&input[..part_len]);
        md5_transform(&mut context.state, &context.buffer);
        
        let mut i = part_len;
        while i + 64 <= input.len() {
            let mut block = [0u8; 64];
            block.copy_from_slice(&input[i..i + 64]);
            md5_transform(&mut context.state, &block);
            i += 64;
        }
        index = 0;
    } else {
        context.buffer[index..index + input.len()].copy_from_slice(input);
        return;
    }
    
    if input.len() > index {
        context.buffer[..input.len() - index].copy_from_slice(&input[index..]);
    }
}

fn md5_final(context: &mut MD5Context) -> [u8; 16] {
    let mut bits = [0u8; 8];
    bits[..4].copy_from_slice(&context.count[0].to_le_bytes());
    bits[4..].copy_from_slice(&context.count[1].to_le_bytes());
    
    let index = ((context.count[0] >> 3) & 0x3F) as usize;
    let pad_len = if index < 56 { 56 - index } else { 120 - index };
    let mut padding = vec![0x80];
    padding.resize(pad_len, 0);
    md5_update(context, &padding);
    md5_update(context, &bits);
    
    let mut digest = [0u8; 16];
    for (i, &word) in context.state.iter().enumerate() {
        digest[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    digest
}

fn get_hash(input: &str) -> String {
    let mut context = MD5Context {
        state: [0; 4],
        count: [0; 2],
        buffer: [0; 64],
    };
    md5_init(&mut context);
    md5_update(&mut context, input.as_bytes());
    let digest = md5_final(&mut context);
    digest.iter().map(|b| format!("{:02x}", b)).collect()
}

fn get_open_doors(hash_str: &str) -> [bool; 4] {
    let door_chars = "bcdef";
    let mut doors = [false; 4];
    for (i, c) in hash_str.chars().take(4).enumerate() {
        doors[i] = door_chars.contains(c);
    }
    doors
}

fn is_valid_move(x: i32, y: i32, direction: char) -> bool {
    match direction {
        'U' => y > 0,
        'D' => y < 3,
        'L' => x > 0,
        'R' => x < 3,
        _ => false,
    }
}

fn find_shortest_path(passcode: &str) -> String {
    let directions = ['U', 'D', 'L', 'R'];
    let dx = [0, 0, -1, 1];
    let dy = [-1, 1, 0, 0];
    
    let mut queue = VecDeque::new();
    queue.push_back((0, 0, String::new()));
    
    while let Some((x, y, path)) = queue.pop_front() {
        if x == 3 && y == 3 {
            return path;
        }
        
        let hash_input = format!("{}{}", passcode, path);
        let hash_str = get_hash(&hash_input);
        let open_doors = get_open_doors(&hash_str);
        
        for i in 0..4 {
            if open_doors[i] && is_valid_move(x, y, directions[i]) {
                let new_x = x + dx[i];
                let new_y = y + dy[i];
                let new_path = format!("{}{}", path, directions[i]);
                queue.push_back((new_x, new_y, new_path));
            }
        }
    }
    String::new()
}

fn main() {
    let passcode = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let shortest_path = find_shortest_path(&passcode);
    println!("{}", shortest_path);
}
