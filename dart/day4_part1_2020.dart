import 'dart:io';

void main() {
  List<String> requiredFields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'];
  
  List<String> passports = File('input.txt').readAsStringSync().split('\n\n');
  
  int validPassports = 0;
  
  for (String passport in passports) {
    List<String> fields = passport.split(RegExp(r'\s+'));
    
    List<String> fieldKeys = [];
    for (String field in fields) {
      fieldKeys.add(field.split(':')[0]);
    }
    
    bool isValid = true;
    for (String requiredField in requiredFields) {
      if (!fieldKeys.contains(requiredField)) {
        isValid = false;
        break;
      }
    }
    
    if (isValid) {
      validPassports++;
    }
  }
  
  print(validPassports);
}