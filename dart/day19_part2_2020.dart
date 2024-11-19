
import 'dart:io';
import 'dart:convert';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final result = solve(input);
  print(result);
}

int solve(String input) {
  final parts = input.split('\n\n');
  final rules = <int, List<List<int>>>{};
  final messages = parts[1].split('\n');

  for (final r in parts[0].split('\n')) {
    if (r.contains(RegExp(r'[a-z]'))) {
      final match = RegExp(r'(\d+): "(\w)"').firstMatch(r)!;
      rules[int.parse(match.group(1)!)] = [[int.parse(match.group(2)!.codeUnitAt(0).toString())]];
    } else {
      final match = RegExp(r'(\d+): (.+)').firstMatch(r)!;
      final key = int.parse(match.group(1)!);
      rules[key] = match.group(2)!.split(' | ').map((e) => e.split(' ').map(int.parse).toList()).toList();
    }
  }

  final resolved42 = resolve(rules, 42).toSet();
  final resolved31 = resolve(rules, 31).toSet();

  final part42 = '(${resolved42.join('|')})';
  final part31 = '(${resolved31.join('|')})';
  final rule8String = '(${part42})+';

  int matchRuleZero = 0;
  for (final m in messages) {
    for (int i = 1; i < 10; i++) {
      final pattern = RegExp('^$rule8String${part42}{${i}}${part31}{${i}}\$');
      if (pattern.hasMatch(m)) {
        matchRuleZero++;
        break;
      }
    }
  }

  return matchRuleZero;
}

Set<String> resolve(Map<int, List<List<int>>> rules, int entry) {
  final resolved = <String>{};
  if (rules[entry]!.first.first < 32) {
    resolved.add(String.fromCharCode(rules[entry]!.first.first));
    return resolved;
  }

  for (final option in rules[entry]!) {
    final nestedResolveVals = option.map((e) => resolve(rules, e)).toList();
    final newResolved = <String>{};
    for (final nextPiece in nestedResolveVals[0]) {
      if (nestedResolveVals.length == 1) {
        newResolved.add(nextPiece);
      } else {
        for (final prev in nestedResolveVals[1]) {
          newResolved.add('$nextPiece$prev');
        }
      }
    }
    resolved.addAll(newResolved);
  }

  return resolved;
}
