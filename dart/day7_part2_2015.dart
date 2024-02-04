import 'dart:io';

void main() {
  var file = File('input.txt');
  var input = file.readAsStringSync().trim();

  print(someAssemblyRequired(input));
}

int someAssemblyRequired(String input) {
  var wireToRule = <String, String>{};

  for (var inst in input.split('\n')) {
    var parts = inst.split(' -> ');
    wireToRule[parts[1]] = parts[0];
  }

  var aSignal = memoDFS(wireToRule, 'a', {});

  wireToRule['b'] = aSignal.toString();
  return memoDFS(wireToRule, 'a', {});
}

int memoDFS(Map<String, String> graph, String entry, Map<String, int> memo) {
  if (memo.containsKey(entry)) {
    return memo[entry]!;
  }

  if (RegExp(r'[0-9]').hasMatch(entry)) {
    return int.parse(entry);
  }

  var sourceRule = graph[entry]!;
  var parts = sourceRule.split(' ');

  late int result;
  switch (parts.length) {
    case 1:
      result = memoDFS(graph, parts[0], memo);
      break;
    case 2:
      var start = memoDFS(graph, parts[1], memo);
      result = (65535 ^ start);
      break;
    case 3:
      switch (parts[1]) {
        case 'AND':
          result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo);
          break;
        case 'OR':
          result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo);
          break;
        case 'LSHIFT':
          result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo);
          break;
        case 'RSHIFT':
          result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo);
          break;
      }
  }

  memo[entry] = result;
  return result;
}