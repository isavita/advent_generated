import 'dart:convert';
import 'dart:io';

int sumNumbers(dynamic obj) {
  if (obj is int) {
    return obj;
  } else if (obj is List) {
    return obj.map((e) => sumNumbers(e)).reduce((a, b) => a + b);
  } else if (obj is Map) {
    if (obj.containsValue("red")) {
      return 0;
    }
    return obj.values.map((e) => sumNumbers(e)).reduce((a, b) => a + b);
  }
  return 0;
}

void main() {
  String content = File('input.txt').readAsStringSync();
  dynamic json = jsonDecode(content);
  
  int sumPart1 = sumNumbers(json);
  print(sumPart1);
  
  int sumPart2 = sumNumbers(json);
  print(sumPart2);
}