
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  
  while (input.length < 272) {
    String a = input;
    String b = a.split('').reversed.map((c) => c == '0' ? '1' : '0').join('');
    input = '$a' '0' '$b';
  }
  
  String data = input.substring(0, 272);
  
  String checksum = generateChecksum(data);
  
  print(checksum);
}

String generateChecksum(String data) {
  String checksum = '';
  
  do {
    checksum = '';
    for (int i = 0; i < data.length - 1; i += 2) {
      if (data[i] == data[i + 1]) {
        checksum += '1';
      } else {
        checksum += '0';
      }
    }
    
    data = checksum;
  } while (checksum.length % 2 == 0);
  
  return checksum;
}
