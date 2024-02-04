
import 'dart:convert';
import 'dart:io';

int sumNumbers(dynamic obj) {
  if (obj is int) {
    return obj;
  } else if (obj is List) {
    return obj.map((e) => sumNumbers(e)).reduce((a, b) => a + b);
  } else if (obj is Map) {
    return obj.values.map((e) => sumNumbers(e)).reduce((a, b) => a + b);
  } else {
    return 0;
  }
}

void main() {
  File file = File('input.txt');
  String input = file.readAsStringSync();
  dynamic data = json.decode(input);
  
  int result = sumNumbers(data);
  print(result);
}
