
import 'dart:io';

class Rule {
  String category;
  String operator;
  int num;
  String workflowName;

  Rule(this.category, this.operator, this.num, this.workflowName);
}

class Part {
  int x, m, a, s;

  Part(this.x, this.m, this.a, this.s);
}

List<String> readFile(String fileName) {
  return File(fileName).readAsStringSync().trim().split('\n');
}

Map<String, List<Rule>> parseWorkflows(List<String> lines) {
  Map<String, List<Rule>> workflows = {};
  for (var line in lines) {
    if (line.isEmpty) break;
    var parts = line.split('{');
    var workflowName = parts[0];
    var rulesStr = parts[1].substring(0, parts[1].length - 1).split(',');
    List<Rule> rules = [];
    for (var ruleStr in rulesStr) {
      if (ruleStr.contains(':')) {
        var idx = ruleStr.indexOf(':');
        var category = ruleStr[0];
        var operator = ruleStr[1];
        var num = int.parse(ruleStr.substring(2, idx));
        var workflowName = ruleStr.substring(idx + 1);
        rules.add(Rule(category, operator, num, workflowName));
      } else {
        rules.add(Rule('', '', 0, ruleStr));
      }
    }
    workflows[workflowName] = rules;
  }
  return workflows;
}

List<Part> parseParts(List<String> lines) {
  List<Part> parts = [];
  for (var i = 0; i < lines.length; i++) {
    if (lines[i].isEmpty) {
      for (var j = i + 1; j < lines.length; j++) {
        var part = RegExp(r'{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}').firstMatch(lines[j]);
        if (part != null) {
          parts.add(Part(
            int.parse(part.group(1)!),
            int.parse(part.group(2)!),
            int.parse(part.group(3)!),
            int.parse(part.group(4)!),
          ));
        }
      }
      break;
    }
  }
  return parts;
}

bool applyWorkflow(Part part, Map<String, List<Rule>> workflows, String workflowName) {
  if (workflowName == 'A') return true;
  if (workflowName == 'R') return false;

  for (var rule in workflows[workflowName]!) {
    var rating = rule.category == 'x' ? part.x : rule.category == 'm' ? part.m : rule.category == 'a' ? part.a : part.s;
    bool isValid = false;
    if (rule.operator == '>') {
      isValid = rating > rule.num;
    } else if (rule.operator == '<') {
      isValid = rating < rule.num;
    } else {
      isValid = true;
    }
    if (isValid) {
      return applyWorkflow(part, workflows, rule.workflowName);
    }
  }
  return false;
}

int solve(List<String> input) {
  var workflows = parseWorkflows(input);
  var parts = parseParts(input);
  int res = 0;
  for (var part in parts) {
    if (applyWorkflow(part, workflows, 'in')) {
      res += part.x + part.m + part.a + part.s;
    }
  }
  return res;
}

void main() {
  var input = readFile('input.txt');
  print(solve(input));
}
