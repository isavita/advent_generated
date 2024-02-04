use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let passports: Vec<&str> = input.trim().split("\n\n").collect();

    let mut valid_passports = 0;

    for passport in passports {
        let fields: Vec<&str> = passport.split_whitespace().collect();
        let mut valid_fields = 0;

        for field in fields {
            let key_value: Vec<&str> = field.split(":").collect();
            let key = key_value[0];
            let value = key_value[1];

            match key {
                "byr" => {
                    let year: i32 = value.parse().unwrap();
                    if year >= 1920 && year <= 2002 {
                        valid_fields += 1;
                    }
                }
                "iyr" => {
                    let year: i32 = value.parse().unwrap();
                    if year >= 2010 && year <= 2020 {
                        valid_fields += 1;
                    }
                }
                "eyr" => {
                    let year: i32 = value.parse().unwrap();
                    if year >= 2020 && year <= 2030 {
                        valid_fields += 1;
                    }
                }
                "hgt" => {
                    if value.contains("cm") {
                        let height: i32 = value.trim_matches(|c: char| !c.is_numeric()).parse().unwrap();
                        if height >= 150 && height <= 193 {
                            valid_fields += 1;
                        }
                    } else if value.contains("in") {
                        let height: i32 = value.trim_matches(|c: char| !c.is_numeric()).parse().unwrap();
                        if height >= 59 && height <= 76 {
                            valid_fields += 1;
                        }
                    }
                }
                "hcl" => {
                    if value.len() == 7 && &value[0..1] == "#" {
                        let valid_chars = "0123456789abcdef";
                        if value[1..].chars().all(|c| valid_chars.contains(c)) {
                            valid_fields += 1;
                        }
                    }
                }
                "ecl" => {
                    let valid_colors = vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
                    if valid_colors.contains(&value) {
                        valid_fields += 1;
                    }
                }
                "pid" => {
                    if value.len() == 9 && value.chars().all(char::is_numeric) {
                        valid_fields += 1;
                    }
                }
                _ => {}
            }
        }

        if valid_fields == 7 {
            valid_passports += 1;
        }
    }

    println!("{}", valid_passports);
}