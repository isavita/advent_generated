import 'dart:io';

class BagRule {
  String color;
  int count;

  BagRule(this.color, this.count);
}

void main() {
  var file = File('input.txt');
  var rules = <String, List<BagRule>>{};
  var ruleRegex = RegExp(r'(\d+) (\w+ \w+) bags?[,.]');

  file.readAsLinesSync().forEach((line) {
    var parts = line.split(' bags contain ');
    var container = parts[0];
    var contents = parts[1];

    if (contents == 'no other bags.') {
      return;
    }

    ruleRegex.allMatches(contents).forEach((match) {
      var count = int.parse(match.group(1)!);
      rules.putIfAbsent(container, () => []).add(BagRule(match.group(2)!, count));
    });
  });

  var totalBags = countBags('shiny gold', rules) - 1;
  print(totalBags);
}

int countBags(String color, Map<String, List<BagRule>> rules) {
  var count = 1;
  rules[color]?.forEach((rule) {
    count += rule.count * countBags(rule.color, rules);
  });
  return count;
}