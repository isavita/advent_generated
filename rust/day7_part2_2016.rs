
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let tls_count = count_tls_ips("input.txt");
    println!("Number of TLS supporting IPs: {}", tls_count);

    let ssl_count = count_ssl_ips("input.txt");
    println!("Number of SSL supporting IPs: {}", ssl_count);
}

fn count_tls_ips(filename: &str) -> usize {
    let mut count = 0;
    if let Ok(lines) = read_lines(filename) {
        for line in lines {
            if let Ok(ip) = line {
                if supports_tls(&ip) {
                    count += 1;
                }
            }
        }
    }
    count
}

fn supports_tls(ip: &str) -> bool {
    let mut supernet_abbas = false;
    let mut hypernet_abbas = false;
    let mut in_hypernet = false;
    let mut current_seq = String::new();

    for c in ip.chars() {
        match c {
            '[' => {
                if !current_seq.is_empty() {
                    if has_abba(&current_seq){
                        supernet_abbas = true;
                    }
                }
                current_seq.clear();
                in_hypernet = true;
            }
            ']' => {
                 if has_abba(&current_seq){
                        hypernet_abbas = true;
                }
                current_seq.clear();
                in_hypernet = false;
            }
            _ => current_seq.push(c),
        }
    }

      if !current_seq.is_empty() {
        if has_abba(&current_seq){
            supernet_abbas = true
        }
    }
    supernet_abbas && !hypernet_abbas
}

fn has_abba(s: &str) -> bool {
    if s.len() < 4 {
        return false;
    }
    for i in 0..=s.len() - 4 {
        let sub = &s[i..i + 4];
        if sub[0..1] != sub[1..2] && sub[0..1] == sub[3..4] && sub[1..2] == sub[2..3] {
            return true;
        }
    }
    false
}

fn count_ssl_ips(filename: &str) -> usize {
    let mut count = 0;
    if let Ok(lines) = read_lines(filename) {
        for line in lines {
            if let Ok(ip) = line {
                if supports_ssl(&ip) {
                    count += 1;
                }
            }
        }
    }
    count
}


fn supports_ssl(ip: &str) -> bool {
     let mut supernet_abas = Vec::new();
    let mut hypernet_babs = Vec::new();

    let mut in_hypernet = false;
    let mut current_seq = String::new();

    for c in ip.chars() {
        match c {
            '[' => {
                if !current_seq.is_empty() {
                    find_abas(&current_seq,&mut supernet_abas);
                }
                current_seq.clear();
                in_hypernet = true;
            }
            ']' => {
                 find_babs(&current_seq, &mut hypernet_babs);
                current_seq.clear();
                in_hypernet = false;
            }
            _ => current_seq.push(c),
        }
    }

    if !current_seq.is_empty() {
         find_abas(&current_seq,&mut supernet_abas);
    }

   supernet_abas.iter().any(|aba| hypernet_babs.contains(&reverse_aba(aba)))
}


fn find_abas(s: &str, abas: &mut Vec<String>) {
    if s.len() < 3 {
        return;
    }
    for i in 0..=s.len() - 3 {
         let sub = &s[i..i + 3];
        if sub[0..1] == sub[2..3] && sub[0..1] != sub[1..2] {
            abas.push(sub.to_string());
        }
    }
}


fn find_babs(s: &str, babs: &mut Vec<String>) {
    if s.len() < 3 {
        return;
    }
    for i in 0..=s.len() - 3 {
        let sub = &s[i..i + 3];
        if sub[0..1] == sub[2..3] && sub[0..1] != sub[1..2] {
              babs.push(sub.to_string());
        }
    }
}


fn reverse_aba(aba: &str) -> String {
    let chars: Vec<char> = aba.chars().collect();
    format!("{}{}{}", chars[1],chars[0],chars[1])
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
