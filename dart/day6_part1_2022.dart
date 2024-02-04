
import 'dart:io';

void main() {
  var s = File('input.txt').readAsStringSync().trim();
  print(firstNUnique(s, 4));
}

int firstNUnique(String s, int n) {
  for (var i = n; i < s.length; i++) {
    var b = s.substring(i - n, i).split('');
    if (b.toSet().length == b.length) {
      return i;
    }
  }
  return -1;
}
