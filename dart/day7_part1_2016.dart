
import 'dart:io';

void main() {
  List<String> ips = File('input.txt').readAsLinesSync();
  int count = 0;

  for (String ip in ips) {
    List<String> parts = ip.split(RegExp(r'\[|\]'));
    List<String> outside = [];
    List<String> inside = [];

    for (int i = 0; i < parts.length; i++) {
      if (i.isEven) {
        outside.add(parts[i]);
      } else {
        inside.add(parts[i]);
      }
    }

    bool supportsTLS = false;

    for (String s in outside) {
      if (hasABBA(s)) {
        supportsTLS = true;
      }
    }

    for (String s in inside) {
      if (hasABBA(s)) {
        supportsTLS = false;
      }
    }

    if (supportsTLS) {
      count++;
    }
  }

  print(count);
}

bool hasABBA(String s) {
  for (int i = 0; i < s.length - 3; i++) {
    if (s[i] != s[i + 1] && s[i] == s[i + 3] && s[i + 1] == s[i + 2]) {
      return true;
    }
  }
  return false;
}
