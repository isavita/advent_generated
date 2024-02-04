import 'dart:io';

void main() {
  var file = new File('input.txt');
  var sum = 0;

  file.readAsLines().then((List<String> lines) {
    for (var line in lines) {
      var result = evaluate(line);
      sum += result;
    }
    print(sum);
  });
}

int evaluate(String expression) {
  var tokens = tokenize(expression);
  return evaluateTokens(tokens);
}

List<String> tokenize(String expression) {
  expression = expression.replaceAll("(", "( ");
  expression = expression.replaceAll(")", " )");
  return expression.split(" ");
}

int evaluateTokens(List<String> tokens) {
  var ops = [];
  var vals = [];

  for (var i = 0; i < tokens.length; i++) {
    var token = tokens[i];
    switch (token) {
      case "(":
        ops.add(token);
        break;
      case "+":
      case "*":
        while (ops.length > 0 && ops.last != "(") {
          vals = vals.sublist(0, vals.length - 2)
            ..add(applyOp(ops.last, vals[vals.length - 2], vals.last));
          ops.removeLast();
        }
        ops.add(token);
        break;
      case ")":
        while (ops.last != "(") {
          vals = vals.sublist(0, vals.length - 2)
            ..add(applyOp(ops.last, vals[vals.length - 2], vals.last));
          ops.removeLast();
        }
        ops.removeLast(); // Remove the opening '('
        break;
      default:
        var value = int.parse(token);
        vals.add(value);
    }
  }
  while (ops.length > 0) {
    vals = vals.sublist(0, vals.length - 2)
      ..add(applyOp(ops.last, vals[vals.length - 2], vals.last));
    ops.removeLast();
  }
  return vals[0];
}

int applyOp(String op, int a, int b) {
  switch (op) {
    case "+":
      return a + b;
    case "*":
      return a * b;
    default:
      throw "Unknown operator: $op";
  }
}