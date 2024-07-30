import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        eprintln('File not found: $input_file')
        return
    }
    
    content := os.read_file(input_file) or {
        eprintln('Error reading file: $input_file')
        return
    }

    passports := content.split('\n\n')
    valid_count := passports.filter(is_valid_passport).len
    println(valid_count)
}

fn is_valid_passport(passport string) bool {
    required_fields := ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']
    for field in required_fields {
        if !passport.contains(field + ':') {
            return false
        }
    }
    return true
}