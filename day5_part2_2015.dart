
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int niceCount = 0;
  
  for (String line in lines) {
    if (isNice(line)) {
      niceCount++;
    }
  }
  
  print(niceCount);
}

bool isNice(String str) {
  return hasPairTwice(str) && hasRepeatWithOneInBetween(str);
}

bool hasPairTwice(String str) {
  for (int i = 0; i < str.length - 1; i++) {
    String pair = str.substring(i, i + 2);
    if (str.substring(i + 2).contains(pair)) {
      return true;
    }
  }
  return false;
}

bool hasRepeatWithOneInBetween(String str) {
  for (int i = 0; i < str.length - 2; i++) {
    if (str[i] == str[i + 2]) {
      return true;
    }
  }
  return false;
}
