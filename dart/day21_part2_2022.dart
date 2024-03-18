import 'dart:io';
import 'dart:math';

class Monkey {
  String name;
  int val;
  bool hasVal;
  Monkey? left;
  Monkey? right;
  String op;

  Monkey(this.name, this.val, this.hasVal, this.left, this.right, this.op);

  int? solve() {
    if (hasVal) return val;

    if (left != null && right != null) {
      int? leftVal = left?.solve();
      int? rightVal = right?.solve();

      if (leftVal != null && rightVal != null) {
        switch (op) {
          case '+':
            return leftVal + rightVal;
          case '-':
            return leftVal - rightVal;
          case '*':
            return leftVal * rightVal;
          case '/':
            return leftVal ~/ rightVal;
          case '==':
            return leftVal == rightVal ? 0 : 1;
        }
      }
    }
    return null;
  }

  int expect(int x) {
    if (name == 'humn') return x;

    int? leftVal = left?.solve();
    int? rightVal = right?.solve();

    if (leftVal == null) {
      switch (op) {
        case '+':
          return left!.expect(x - rightVal!);
        case '-':
          return left!.expect(x + rightVal!);
        case '*':
          return left!.expect(x ~/ rightVal!);
        case '/':
          return left!.expect(x * rightVal!);
        case '==':
          return left!.expect(rightVal!);
      }
    }

    if (rightVal == null) {
      switch (op) {
        case '+':
          return right!.expect(x - leftVal!);
        case '-':
          return right!.expect(leftVal! - x);
        case '*':
          return right!.expect(x ~/ leftVal!);
        case '/':
          return right!.expect(leftVal! ~/ x);
        case '==':
          return right!.expect(leftVal!);
      }
    }

    throw Exception('Impossible');
  }
}

void main() {
  Map<String, Monkey> index = parse();

  // set "humn" as unknown
  index['humn']?.hasVal = false;

  // new operator: == returns 0 if equals, 1 otherwise
  index['root']?.op = '==';

  print(index['root']?.expect(0));
}

Map<String, Monkey> parse() {
  Map<String, Monkey> index = {};

  void initMonkey(String s) {
    if (!index.containsKey(s)) {
      index[s] = Monkey(s, 0, false, null, null, '');
    }
  }

  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    List<String> parts = line.split(': ');
    String goal = parts[0];
    initMonkey(goal);

    int? num = int.tryParse(parts[1]);
    if (num != null) {
      index[goal]?.val = num;
      index[goal]?.hasVal = true;
      continue;
    }

    List<String> r = parts[1].split(' ');
    String left = r[0];
    String op = r[1];
    String right = r[2];

    initMonkey(left);
    initMonkey(right);

    index[goal]?.left = index[left];
    index[goal]?.op = op;
    index[goal]?.right = index[right];
  }

  return index;
}