use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let passports: Vec<&str> = input.split("\n\n").collect();

    let mut valid_passports = 0;
    for passport in passports {
        let fields: Vec<&str> = passport.split_whitespace().collect();
        let mut valid_fields = 0;

        for field in fields {
            let key_value: Vec<&str> = field.split(":").collect();
            let key = key_value[0];
            let value = key_value[1];

            match key {
                "byr" | "iyr" | "eyr" | "hgt" | "hcl" | "ecl" | "pid" => valid_fields += 1,
                _ => (),
            }
        }

        if valid_fields == 7 {
            valid_passports += 1;
        }
    }

    println!("{}", valid_passports);
}