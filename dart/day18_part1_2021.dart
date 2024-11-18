
import 'dart:io';

class SnailNumber {
  int? value;
  SnailNumber? left;
  SnailNumber? right;

  SnailNumber({this.value, this.left, this.right});

  bool get isRegular => left == null && right == null;

  SnailNumber add(SnailNumber other) {
    final newNumber = SnailNumber(left: this, right: other);
    return newNumber.reduce();
  }

  SnailNumber reduce() {
    while (true) {
      final explodeResult = explode(0);
      if (explodeResult.didExplode) continue;
      if (!split()) break;
    }
    return this;
  }

  ExplodeResult explode(int depth) {
    if (isRegular) return ExplodeResult(false);

    if (depth == 4) {
      final leftValue = left!.value!;
      final rightValue = right!.value!;
      left = null;
      right = null;
      value = 0;
      return ExplodeResult(true, leftValue, rightValue);
    }

    var result = left!.explode(depth + 1);
    if (result.didExplode) {
      if (result.rightValue != null && right != null) {
        right!.addLeft(result.rightValue!);
      }
      return ExplodeResult(true, result.leftValue, null);
    }

    result = right!.explode(depth + 1);
    if (result.didExplode) {
      if (result.leftValue != null && left != null) {
        left!.addRight(result.leftValue!);
      }
      return ExplodeResult(true, null, result.rightValue);
    }

    return ExplodeResult(false);
  }

  void addLeft(int value) {
    if (isRegular) {
      this.value = (this.value ?? 0) + value;
    } else {
      left!.addLeft(value);
    }
  }

  void addRight(int value) {
    if (isRegular) {
      this.value = (this.value ?? 0) + value;
    } else {
      right!.addRight(value);
    }
  }

  bool split() {
    if (isRegular) {
      if ((value ?? 0) >= 10) {
        left = SnailNumber(value: (value! ~/ 2));
        right = SnailNumber(value: ((value! + 1) ~/ 2));
        value = null;
        return true;
      }
      return false;
    }
    return left!.split() || right!.split();
  }

  int magnitude() {
    if (isRegular) return value!;
    return 3 * left!.magnitude() + 2 * right!.magnitude();
  }

  static SnailNumber parse(String input) {
    input = input.trim();
    if (input[0] != '[') {
      return SnailNumber(value: int.parse(input));
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
      if (splitIndex != 0) break;
    }

    final left = parse(input.substring(1, splitIndex));
    final right = parse(input.substring(splitIndex + 1, input.length - 1));
    return SnailNumber(left: left, right: right);
  }
}

class ExplodeResult {
  final bool didExplode;
  final int? leftValue;
  final int? rightValue;

  ExplodeResult(this.didExplode, [this.leftValue, this.rightValue]);
}

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  if (lines.isEmpty) {
    print('No snailfish numbers found in the file.');
    return;
  }

  final snailNumbers = lines.map(SnailNumber.parse).toList();
  var result = snailNumbers[0];

  for (int i = 1; i < snailNumbers.length; i++) {
    result = result.add(snailNumbers[i]);
  }

  print(result.magnitude());
}
