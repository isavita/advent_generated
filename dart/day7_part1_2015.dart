import 'dart:io';
import 'dart:math';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  print(someAssemblyRequired(input));
}

int someAssemblyRequired(String input) {
  final wireToRule = <String, String>{};

  for (final inst in input.split('\n')) {
    final parts = inst.split(' -> ');
    wireToRule[parts[1]] = parts[0];
  }

  final aSignal = memoDFS(wireToRule, 'a', <String, int>{});
  return aSignal;
}

int memoDFS(Map<String, String> graph, String entry, Map<String, int> memo) {
  if (memo.containsKey(entry)) {
    return memo[entry]!;
  }

  if (int.tryParse(entry) != null) {
    return int.parse(entry);
  }

  final sourceRule = graph[entry]!;
  final parts = sourceRule.split(' ');

  int result;
  switch (parts.length) {
    case 1:
      result = memoDFS(graph, parts[0], memo);
      break;
    case 2:
      if (parts[0] == 'NOT') {
        final start = memoDFS(graph, parts[1], memo);
        result = ~start & 0xFFFF;
      } else {
        throw Exception('Invalid instruction: $sourceRule');
      }
      break;
    case 3:
      final left = memoDFS(graph, parts[0], memo);
      final right = memoDFS(graph, parts[2], memo);
      switch (parts[1]) {
        case 'AND':
          result = left & right;
          break;
        case 'OR':
          result = left | right;
          break;
        case 'LSHIFT':
          result = left << right;
          break;
        case 'RSHIFT':
          result = left >> right;
          break;
        default:
          throw Exception('Invalid instruction: $sourceRule');
      }
      break;
    default:
      throw Exception('Invalid instruction: $sourceRule');
  }

  memo[entry] = result;
  return result;
}