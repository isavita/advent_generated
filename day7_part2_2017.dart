import 'dart:io';
import 'dart:convert';

class Program {
  int weight;
  List<String> holds;

  Program({required this.weight, this.holds = const []});
}

Map<String, Program> programs = {};

int dfs(String name) {
  Program program = programs[name]!;
  int totalWeight = program.weight;

  Map<int, int> weights = {};
  bool balanced = true;

  for (String child in program.holds) {
    int weight = dfs(child);
    if (weight == 0) {
      balanced = false;
      break;
    }
    totalWeight += weight;
    weights[weight] = (weights[weight] ?? 0) + 1;
  }

  if (!balanced) return 0;

  for (int w1 in weights.keys) {
    for (int w2 in weights.keys) {
      if (w1 != w2 && weights[w1]! < weights[w2]!) {
        String unbalancedProgram = '';
        for (String child in program.holds) {
          if (dfs(child) == w1) {
            unbalancedProgram = child;
            break;
          }
        }
        print(programs[unbalancedProgram]!.weight + (w2 - w1));
        return 0;
      }
    }
  }

  return totalWeight;
}

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();

  RegExp re = RegExp(r'[a-z]+|\d+');

  for (String line in lines) {
    List<Match> matches = re.allMatches(line).toList();
    String name = matches[0][0]!;
    int weight = int.parse(matches[1][0]!);

    Program program = Program(weight: weight);
    if (matches.length > 2) {
      program.holds = matches.sublist(2).map((e) => e[0]!).toList();
    }
    programs[name] = program;
  }

  String root = "dtacyn";

  dfs(root);
}