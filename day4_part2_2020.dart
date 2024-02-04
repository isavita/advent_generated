import 'dart:io';

void main() {
  List<String> passports = File('input.txt').readAsStringSync().split('\n\n');
  
  int validPassports = 0;
  
  for (String passport in passports) {
    List<String> fields = passport.split(RegExp(r'\s+'));
    Set<String> fieldKeys = fields.map((field) => field.split(':')[0]).toSet();
    
    if (fieldKeys.containsAll(['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'])) {
      bool valid = true;
      
      for (String field in fields) {
        List<String> parts = field.split(':');
        String key = parts[0];
        String value = parts[1];
        
        switch (key) {
          case 'byr':
            if (int.parse(value) < 1920 || int.parse(value) > 2002) {
              valid = false;
            }
            break;
          case 'iyr':
            if (int.parse(value) < 2010 || int.parse(value) > 2020) {
              valid = false;
            }
            break;
          case 'eyr':
            if (int.parse(value) < 2020 || int.parse(value) > 2030) {
              valid = false;
            }
            break;
          case 'hgt':
            if (value.endsWith('cm')) {
              int height = int.parse(value.substring(0, value.length - 2));
              if (height < 150 || height > 193) {
                valid = false;
              }
            } else if (value.endsWith('in')) {
              int height = int.parse(value.substring(0, value.length - 2));
              if (height < 59 || height > 76) {
                valid = false;
              }
            } else {
              valid = false;
            }
            break;
          case 'hcl':
            if (!RegExp(r'^#[0-9a-f]{6}$').hasMatch(value)) {
              valid = false;
            }
            break;
          case 'ecl':
            if (!['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'].contains(value)) {
              valid = false;
            }
            break;
          case 'pid':
            if (!RegExp(r'^\d{9}$').hasMatch(value)) {
              valid = false;
            }
            break;
        }
      }
      
      if (valid) {
        validPassports++;
      }
    }
  }
  
  print(validPassports);
}