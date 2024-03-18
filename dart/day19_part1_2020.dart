import 'dart:io';
import 'dart:convert';
import 'dart:math';

Map<int, String> readRules(List<String> lines) {
  Map<int, String> rules = {};
  for (String line in lines) {
    if (line.isEmpty) break;
    List<String> parts = line.split(': ');
    rules[int.parse(parts[0])] = parts[1].replaceAll('"', '');
  }
  return rules;
}

String constructPattern(Map<int, String> rules, int index) {
  if (rules[index]!.contains('|')) {
    List<String> subrules = rules[index]!.split(' | ');
    List<String> parts = [];
    for (String subrule in subrules) {
      parts.add(constructSubPattern(rules, subrule));
    }
    return '(${parts.join('|')})';
  }
  return constructSubPattern(rules, rules[index]!);
}

String constructSubPattern(Map<int, String> rules, String subrule) {
  if (subrule == 'a' || subrule == 'b') return subrule;
  List<String> subIdxs = subrule.split(' ');
  String pattern = '';
  for (String idx in subIdxs) {
    pattern += constructPattern(rules, int.parse(idx));
  }
  return pattern;
}

int countMatches(List<String> messages, String pattern) {
  RegExp re = RegExp('^$pattern\$');
  return messages.where((message) => re.hasMatch(message)).length;
}

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  Map<int, String> rules = readRules(lines);
  String pattern = constructPattern(rules, 0);
  List<String> messages = lines.sublist(lines.indexOf('') + 1);
  int count = countMatches(messages, pattern);
  print('The number of messages that completely match rule 0 is: $count');
}