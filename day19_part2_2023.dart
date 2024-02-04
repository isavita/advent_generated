
import 'dart:io';

class Rule {
  late String workflowName;
  late String category;
  late String operator;
  late int num;

  Rule(this.workflowName, this.category, this.operator, this.num);
}

class Workflow {
  late String name;
  late List<Rule> rules;

  Workflow(this.name, this.rules);
}

class Part {
  late int x;
  late int m;
  late int a;
  late int s;

  Part(this.x, this.m, this.a, this.s);
}

class Interval {
  late int start;
  late int end;

  Interval(this.start, this.end);
}

class PartInterval {
  late Interval x;
  late Interval m;
  late Interval a;
  late Interval s;

  PartInterval(this.x, this.m, this.a, this.s);
}

List<String> readFile(String fileName) {
  File file = File(fileName);
  String contents = file.readAsStringSync();
  return contents.trim().split('\n');
}

List<Workflow> parseWorkflows(List<String> input) {
  List<Workflow> workflows = [];
  int i = 0;
  for (; input[i] != ''; i++) {
    workflows.add(parseWorkflow(input[i]));
  }
  return workflows;
}

Workflow parseWorkflow(String line) {
  int idx = line.indexOf('{');
  String workflowName = line.substring(0, idx);
  List<Rule> rules = [];
  List<String> rulesStr = line.substring(idx + 1, line.length - 1).split(',');
  for (String ruleStr in rulesStr) {
    int idx = ruleStr.indexOf(':');
    if (idx == -1) {
      rules.add(Rule(ruleStr, '', '', 0));
    } else {
      String category = ruleStr[0];
      String operator = ruleStr[1];
      int num = int.parse(ruleStr.substring(2, idx));
      String workflowName = ruleStr.substring(idx + 1);
      rules.add(Rule(workflowName, category, operator, num));
    }
  }
  return Workflow(workflowName, rules);
}

Part parsePart(String line) {
  RegExp regExp = RegExp(r'{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}');
  RegExpMatch match = regExp.firstMatch(line)!;
  int x = int.parse(match.group(1)!);
  int m = int.parse(match.group(2)!);
  int a = int.parse(match.group(3)!);
  int s = int.parse(match.group(4)!);
  return Part(x, m, a, s);
}

int applyWorkflow(Part part, List<Workflow> workflows, String workflowName) {
  if (workflowName == 'A') {
    return 1;
  }
  if (workflowName == 'R') {
    return 0;
  }
  for (Workflow workflow in workflows) {
    if (workflow.name == workflowName) {
      for (Rule rule in workflow.rules) {
        int rating = 0;
        switch (rule.category) {
          case 'x':
            rating = part.x;
            break;
          case 'm':
            rating = part.m;
            break;
          case 'a':
            rating = part.a;
            break;
          case 's':
            rating = part.s;
            break;
        }
        bool isValid = false;
        switch (rule.operator) {
          case '>':
            isValid = rating > rule.num;
            break;
          case '<':
            isValid = rating < rule.num;
            break;
          default:
            isValid = true;
        }
        if (isValid) {
          return applyWorkflow(part, workflows, rule.workflowName);
        }
      }
    }
  }
  return 0;
}

int applyWorkflowInterval(PartInterval partInterval, List<Workflow> workflows, String workflowName) {
  if (workflowName == 'A') {
    int res = 1;
    res *= partInterval.x.end - partInterval.x.start + 1;
    res *= partInterval.m.end - partInterval.m.start + 1;
    res *= partInterval.a.end - partInterval.a.start + 1;
    res *= partInterval.s.end - partInterval.s.start + 1;
    return res;
  }
  if (workflowName == 'R') {
    return 0;
  }
  int res = 0;
  for (Workflow workflow in workflows) {
    if (workflow.name == workflowName) {
      for (Rule rule in workflow.rules) {
        Interval ratingInterval = Interval(0, 0);
        switch (rule.category) {
          case 'x':
            ratingInterval = partInterval.x;
            break;
          case 'm':
            ratingInterval = partInterval.m;
            break;
          case 'a':
            ratingInterval = partInterval.a;
            break;
          case 's':
            ratingInterval = partInterval.s;
            break;
        }
        Interval validRatingInterval = Interval(0, 0);
        Interval invalidRatingInterval = Interval(0, 0);
        switch (rule.operator) {
          case '>':
            invalidRatingInterval = Interval(ratingInterval.start, rule.num);
            validRatingInterval = Interval(rule.num + 1, ratingInterval.end);
            break;
          case '<':
            validRatingInterval = Interval(ratingInterval.start, rule.num - 1);
            invalidRatingInterval = Interval(rule.num, ratingInterval.end);
            break;
          default:
            validRatingInterval = ratingInterval;
        }
        PartInterval newPartInterval = PartInterval(partInterval.x, partInterval.m, partInterval.a, partInterval.s);
        switch (rule.category) {
          case 'x':
            newPartInterval.x = validRatingInterval;
            break;
          case 'm':
            newPartInterval.m = validRatingInterval;
            break;
          case 'a':
            newPartInterval.a = validRatingInterval;
            break;
          case 's':
            newPartInterval.s = validRatingInterval;
            break;
        }
        res += applyWorkflowInterval(newPartInterval, workflows, rule.workflowName);
        switch (rule.category) {
          case 'x':
            partInterval.x = invalidRatingInterval;
            break;
          case 'm':
            partInterval.m = invalidRatingInterval;
            break;
          case 'a':
            partInterval.a = invalidRatingInterval;
            break;
          case 's':
            partInterval.s = invalidRatingInterval;
            break;
        }
      }
    }
  }
  return res;
}

int solve(List<String> input) {
  String startWorkflow = 'in';
  int minRating = 1;
  int maxRating = 4000;

  List<Workflow> workflows = parseWorkflows(input);
  PartInterval partInterval = PartInterval(Interval(minRating, maxRating), Interval(minRating, maxRating), Interval(minRating, maxRating), Interval(minRating, maxRating));

  int res = applyWorkflowInterval(partInterval, workflows, startWorkflow);

  return res;
}

void main() {
  List<String> input = readFile('input.txt');
  print(solve(input));
}
