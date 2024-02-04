import 'dart:io';

void main() {
  var sum = 0;
  var file = File('input.txt');
  var lines = file.readAsLinesSync();
  
  for (var line in lines) {
    sum += fromSnafu(line);
  }
  
  print(toSnafu(sum));
}

int fromSnafu(String s) {
  var n = 0;
  for (var i = 0; i < s.length; i++) {
    n *= 5;
    switch (s[i]) {
      case '=':
        n -= 2;
        break;
      case '-':
        n--;
        break;
      default:
        n += int.parse(s[i]);
    }
  }
  return n;
}

String toSnafu(int n) {
  var b = <String>[];
  while (n > 0) {
    switch (n % 5) {
      case 3:
        n += 5;
        b.add('=');
        break;
      case 4:
        n += 5;
        b.add('-');
        break;
      default:
        b.add((n % 5).toString());
    }
    n ~/= 5;
  }
  
  for (var i = 0; i < b.length ~/ 2; i++) {
    var temp = b[i];
    b[i] = b[b.length - 1 - i];
    b[b.length - 1 - i] = temp;
  }
  
  return b.join();
}