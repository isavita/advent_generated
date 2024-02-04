import 'dart:io';

void main() {
  var file = new File('input.txt');
  var contents = file.readAsStringSync();
  var operations = contents.trim().split('\n');

  var password = 'abcdefgh';

  for (var op in operations) {
    password = applyOperation(op, password);
  }

  print(password);
}

String applyOperation(String op, String password) {
  var fields = op.split(' ');
  switch (fields[0]) {
    case 'swap':
      switch (fields[1]) {
        case 'position':
          var x = int.parse(fields[2]);
          var y = int.parse(fields[5]);
          password = swapPosition(password, x, y);
          break;
        case 'letter':
          var x = fields[2];
          var y = fields[5];
          password = swapLetter(password, x, y);
          break;
      }
      break;
    case 'rotate':
      switch (fields[1]) {
        case 'left':
          var steps = int.parse(fields[2]);
          password = rotateLeft(password, steps);
          break;
        case 'right':
          var steps = int.parse(fields[2]);
          password = rotateRight(password, steps);
          break;
        case 'based':
          var x = fields[6];
          password = rotateBasedOnPosition(password, x);
          break;
      }
      break;
    case 'reverse':
      var x = int.parse(fields[2]);
      var y = int.parse(fields[4]);
      password = reversePositions(password, x, y);
      break;
    case 'move':
      var x = int.parse(fields[2]);
      var y = int.parse(fields[5]);
      password = movePosition(password, x, y);
      break;
  }
  return password;
}

String swapPosition(String password, int x, int y) {
  var chars = password.split('');
  var temp = chars[x];
  chars[x] = chars[y];
  chars[y] = temp;
  return chars.join();
}

String swapLetter(String password, String x, String y) {
  var chars = password.split('');
  for (var i = 0; i < chars.length; i++) {
    if (chars[i] == x) {
      chars[i] = y;
    } else if (chars[i] == y) {
      chars[i] = x;
    }
  }
  return chars.join();
}

String rotateLeft(String password, int steps) {
  steps = steps % password.length;
  return password.substring(steps) + password.substring(0, steps);
}

String rotateRight(String password, int steps) {
  steps = steps % password.length;
  return password.substring(password.length - steps) + password.substring(0, password.length - steps);
}

String rotateBasedOnPosition(String password, String x) {
  var index = password.indexOf(x);
  var steps = 1 + index;
  if (index >= 4) {
    steps++;
  }
  return rotateRight(password, steps);
}

String reversePositions(String password, int x, int y) {
  var chars = password.split('');
  while (x < y) {
    var temp = chars[x];
    chars[x] = chars[y];
    chars[y] = temp;
    x++;
    y--;
  }
  return chars.join();
}

String movePosition(String password, int x, int y) {
  var chars = password.split('');
  var char = chars.removeAt(x);
  chars.insert(y, char);
  return chars.join();
}