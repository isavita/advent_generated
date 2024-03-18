import 'dart:io';

class SnailNumber {
  int value;
  SnailNumber? left;
  SnailNumber? right;

  SnailNumber(this.value, [this.left, this.right]);

  bool get isRegular => left == null && right == null;

  SnailNumber add(SnailNumber other) {
    SnailNumber newNumber = SnailNumber(0, this, other);
    return newNumber.reduce();
  }

  SnailNumber reduce() {
    while (true) {
      var exploded = _explode(0);
      if (exploded.item1) {
        continue;
      }
      if (!_split()) {
        break;
      }
    }
    return this;
  }

  Tuple3<bool, int, int> _explode(int depth) {
    if (isRegular) {
      return Tuple3(false, 0, 0);
    }

    if (depth == 4) {
      int leftValue = left!.value;
      int rightValue = right!.value;
      left = null;
      right = null;
      value = 0;
      return Tuple3(true, leftValue, rightValue);
    }

    var exploded = left!._explode(depth + 1);
    if (exploded.item1) {
      if (exploded.item3 > 0 && right != null) {
        right!._addLeft(exploded.item3);
      }
      return Tuple3(true, exploded.item2, 0);
    }

    exploded = right!._explode(depth + 1);
    if (exploded.item1) {
      if (exploded.item2 > 0 && left != null) {
        left!._addRight(exploded.item2);
      }
      return Tuple3(true, 0, exploded.item3);
    }

    return Tuple3(false, 0, 0);
  }

  void _addLeft(int value) {
    if (isRegular) {
      this.value += value;
    } else {
      left!._addLeft(value);
    }
  }

  void _addRight(int value) {
    if (isRegular) {
      this.value += value;
    } else {
      right!._addRight(value);
    }
  }

  bool _split() {
    if (isRegular) {
      if (value >= 10) {
        left = SnailNumber(value ~/ 2);
        right = SnailNumber((value + 1) ~/ 2);
        value = -1;
        return true;
      }
      return false;
    }
    return left!._split() || right!._split();
  }

  int magnitude() {
    if (isRegular) {
      return value;
    }
    return 3 * left!.magnitude() + 2 * right!.magnitude();
  }

  SnailNumber deepCopy() {
    if (isRegular) {
      return SnailNumber(value);
    }
    return SnailNumber(0, left!.deepCopy(), right!.deepCopy());
  }
}

Tuple3<bool, int, int> _explode(SnailNumber sn, int depth) {
  return sn._explode(depth);
}

void main() {
  File file = File('input.txt');
  List<SnailNumber> snailNumbers = [];

  for (String line in file.readAsLinesSync()) {
    snailNumbers.add(parseSnailNumber(line));
  }

  if (snailNumbers.isEmpty) {
    print('No snailfish numbers found in the file.');
    return;
  }

  int largestMagnitude = 0;

  for (int i = 0; i < snailNumbers.length; i++) {
    for (int j = 0; j < snailNumbers.length; j++) {
      if (i == j) {
        continue;
      }

      SnailNumber aCopy = snailNumbers[i].deepCopy();
      SnailNumber bCopy = snailNumbers[j].deepCopy();

      int sum1 = aCopy.add(bCopy.deepCopy()).magnitude();
      int sum2 = bCopy.add(aCopy.deepCopy()).magnitude();

      if (sum1 > largestMagnitude) {
        largestMagnitude = sum1;
      }
      if (sum2 > largestMagnitude) {
        largestMagnitude = sum2;
      }
    }
  }

  print(largestMagnitude);
}

SnailNumber parseSnailNumber(String input) {
  input = input.trim();
  if (input[0] != '[') {
    return SnailNumber(int.parse(input));
  }

  int balance = 0;
  int splitIndex = 0;
  for (int i = 1; i < input.length - 1; i++) {
    switch (input[i]) {
      case '[':
        balance++;
        break;
      case ']':
        balance--;
        break;
      case ',':
        if (balance == 0) {
          splitIndex = i;
          break;
        }
    }
    if (splitIndex != 0) {
      break;
    }
  }

  SnailNumber left = parseSnailNumber(input.substring(1, splitIndex));
  SnailNumber right = parseSnailNumber(input.substring(splitIndex + 1, input.length - 1));
  return SnailNumber(0, left, right);
}

class Tuple3<T1, T2, T3> {
  final T1 item1;
  final T2 item2;
  final T3 item3;

  Tuple3(this.item1, this.item2, this.item3);
}