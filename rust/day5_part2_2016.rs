
use std::fs;

fn md5(input: &[u8]) -> [u8; 16] {
    let mut state = [0x67452301u32, 0xEFCDAB89, 0x98BADCFE, 0x10325476];
    let mut len = 0u64;
    let mut buffer = [0u8; 64];
    let mut offset = 0;

    for &b in input {
        buffer[offset] = b;
        offset += 1;
        len += 1;
        if offset == 64 {
            transform(&mut state, &buffer);
            offset = 0;
        }
    }

    buffer[offset] = 0x80;
    offset += 1;

    if offset > 56 {
        for i in offset..64 {
            buffer[i] = 0;
        }
        transform(&mut state, &buffer);
        offset = 0;
    }

    for i in offset..56 {
        buffer[i] = 0;
    }
    let len_bits = len * 8;
    buffer[56..64].copy_from_slice(&len_bits.to_le_bytes());
    transform(&mut state, &buffer);

    let mut digest = [0u8; 16];
    for (i, &word) in state.iter().enumerate() {
        digest[i * 4..(i + 1) * 4].copy_from_slice(&word.to_le_bytes());
    }
    digest
}

fn transform(state: &mut [u32; 4], block: &[u8; 64]) {
    let mut a = state[0];
    let mut b = state[1];
    let mut c = state[2];
    let mut d = state[3];
    let mut x = [0u32; 16];

    for (i, chunk) in block.chunks(4).enumerate() {
        x[i] = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
    }

    macro_rules! f {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add($b & $c | !$b & $d)
                .wrapping_add($x)
                .wrapping_add($ac)
                .rotate_left($s)
                .wrapping_add($b);
        };
    }
    macro_rules! g {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add($b & $d | $c & !$d)
                .wrapping_add($x)
                .wrapping_add($ac)
                .rotate_left($s)
                .wrapping_add($b);
        };
    }
    macro_rules! h {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add($b ^ $c ^ $d)
                .wrapping_add($x)
                .wrapping_add($ac)
                .rotate_left($s)
                .wrapping_add($b);
        };
    }
    macro_rules! i {
        ($a:expr, $b:expr, $c:expr, $d:expr, $x:expr, $s:expr, $ac:expr) => {
            $a = $a.wrapping_add($c ^ ($b | !$d))
                .wrapping_add($x)
                .wrapping_add($ac)
                .rotate_left($s)
                .wrapping_add($b);
        };
    }

    f!(a, b, c, d, x[0], 7, 0xD76AA478);
    f!(d, a, b, c, x[1], 12, 0xE8C7B756);
    f!(c, d, a, b, x[2], 17, 0x242070DB);
    f!(b, c, d, a, x[3], 22, 0xC1BDCEEE);
    f!(a, b, c, d, x[4], 7, 0xF57C0FAF);
    f!(d, a, b, c, x[5], 12, 0x4787C62A);
    f!(c, d, a, b, x[6], 17, 0xA8304613);
    f!(b, c, d, a, x[7], 22, 0xFD469501);
    f!(a, b, c, d, x[8], 7, 0x698098D8);
    f!(d, a, b, c, x[9], 12, 0x8B44F7AF);
    f!(c, d, a, b, x[10], 17, 0xFFFF5BB1);
    f!(b, c, d, a, x[11], 22, 0x895CD7BE);
    f!(a, b, c, d, x[12], 7, 0x6B901122);
    f!(d, a, b, c, x[13], 12, 0xFD987193);
    f!(c, d, a, b, x[14], 17, 0xA679438E);
    f!(b, c, d, a, x[15], 22, 0x49B40821);

    g!(a, b, c, d, x[1], 5, 0xF61E2562);
    g!(d, a, b, c, x[6], 9, 0xC040B340);
    g!(c, d, a, b, x[11], 14, 0x265E5A51);
    g!(b, c, d, a, x[0], 20, 0xE9B6C7AA);
    g!(a, b, c, d, x[5], 5, 0xD62F105D);
    g!(d, a, b, c, x[10], 9, 0x02441453);
    g!(c, d, a, b, x[15], 14, 0xD8A1E681);
    g!(b, c, d, a, x[4], 20, 0xE7D3FBC8);
    g!(a, b, c, d, x[9], 5, 0x21E1CDE6);
    g!(d, a, b, c, x[14], 9, 0xC33707D6);
    g!(c, d, a, b, x[3], 14, 0xF4D50D87);
    g!(b, c, d, a, x[8], 20, 0x455A14ED);
    g!(a, b, c, d, x[13], 5, 0xA9E3E905);
    g!(d, a, b, c, x[2], 9, 0xFCEFA3F8);
    g!(c, d, a, b, x[7], 14, 0x676F02D9);
    g!(b, c, d, a, x[12], 20, 0x8D2A4C8A);

    h!(a, b, c, d, x[5], 4, 0xFFFA3942);
    h!(d, a, b, c, x[8], 11, 0x8771F681);
    h!(c, d, a, b, x[11], 16, 0x6D9D6122);
    h!(b, c, d, a, x[14], 23, 0xFDE5380C);
    h!(a, b, c, d, x[1], 4, 0xA4BEEA44);
    h!(d, a, b, c, x[4], 11, 0x4BDECFA9);
    h!(c, d, a, b, x[7], 16, 0xF6BB4B60);
    h!(b, c, d, a, x[10], 23, 0xBEBFBC70);
    h!(a, b, c, d, x[13], 4, 0x289B7EC6);
    h!(d, a, b, c, x[0], 11, 0xEAA127FA);
    h!(c, d, a, b, x[3], 16, 0xD4EF3085);
    h!(b, c, d, a, x[6], 23, 0x04881D05);
    h!(a, b, c, d, x[9], 4, 0xD9D4D039);
    h!(d, a, b, c, x[12], 11, 0xE6DB99E5);
    h!(c, d, a, b, x[15], 16, 0x1FA27CF8);
    h!(b, c, d, a, x[2], 23, 0xC4AC5665);

    i!(a, b, c, d, x[0], 6, 0xF4292244);
    i!(d, a, b, c, x[7], 10, 0x432AFF97);
    i!(c, d, a, b, x[14], 15, 0xAB9423A7);
    i!(b, c, d, a, x[5], 21, 0xFC93A039);
    i!(a, b, c, d, x[12], 6, 0x655B59C3);
    i!(d, a, b, c, x[3], 10, 0x8F0CCC92);
    i!(c, d, a, b, x[10], 15, 0xFFEFF47D);
    i!(b, c, d, a, x[1], 21, 0x85845DD1);
    i!(a, b, c, d, x[8], 6, 0x6FA87E4F);
    i!(d, a, b, c, x[15], 10, 0xFE2CE6E0);
    i!(c, d, a, b, x[6], 15, 0xA3014314);
    i!(b, c, d, a, x[13], 21, 0x4E0811A1);
    i!(a, b, c, d, x[4], 6, 0xF7537E82);
    i!(d, a, b, c, x[11], 10, 0xBD3AF235);
    i!(c, d, a, b, x[2], 15, 0x2AD7D2BB);
    i!(b, c, d, a, x[9], 21, 0xEB86D391);

    state[0] = state[0].wrapping_add(a);
    state[1] = state[1].wrapping_add(b);
    state[2] = state[2].wrapping_add(c);
    state[3] = state[3].wrapping_add(d);
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let door_id = input.trim();
    let mut password = [' '; 8];
    let mut found = [false; 8];
    let mut filled = 0;
    let mut i = 0;

    while filled < 8 {
        let input_str = format!("{}{}", door_id, i);
        let digest = md5(input_str.as_bytes());
        if digest[0..2] == [0, 0] && digest[2] < 16 {
            let pos = digest[2] as usize;
            if pos < 8 && !found[pos] {
                found[pos] = true;
                password[pos] = (digest[3] >> 4) as char;
                if password[pos] as u8 >= 10 {
                    password[pos] = (b'a' + password[pos] as u8 - 10) as char;
                } else {
                    password[pos] = (b'0' + password[pos] as u8) as char;
                }
                filled += 1;
            }
        }
        i += 1;
    }

    println!("{}", password.iter().collect::<String>());
}
