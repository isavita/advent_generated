import 'dart:io';
import 'dart:math';

const hashTableSize = 256;

class Step {
  String label;
  int numBox;
  String operation;
  int number;

  Step(this.label, this.numBox, this.operation, this.number);
}

int hashString(String str) {
  int res = 0;
  for (int i = 0; i < str.length; i++) {
    int char = str.codeUnitAt(i);
    res += char;
    res *= 17;
    res %= hashTableSize;
  }
  return res;
}

Step parseStep(String stepStr) {
  int labelEndIndex = stepStr.indexOf(RegExp(r'[-=0123456789]'));
  String label = stepStr.substring(0, labelEndIndex);
  int numBox = hashString(label);
  String operation = stepStr.substring(labelEndIndex, labelEndIndex + 1);
  int number = operation == '=' ? int.parse(stepStr.substring(labelEndIndex + 1)) : 0;
  return Step(label, numBox, operation, number);
}

Map<int, List<Map<String, int>>> getBoxes(List<String> stepsStr) {
  Map<int, List<Map<String, int>>> boxes = {};

  for (String stepStr in stepsStr) {
    Step step = parseStep(stepStr);
    List<Map<String, int>>? boxContents = boxes[step.numBox];
    if (boxContents == null) {
      boxContents = [];
    }

    switch (step.operation) {
      case '-':
        boxContents.removeWhere((content) => content.containsKey(step.label));
        break;
      case '=':
        bool found = false;
        for (Map<String, int> content in boxContents) {
          if (content.containsKey(step.label)) {
            content[step.label] = step.number;
            found = true;
            break;
          }
        }
        if (!found) {
          boxContents.add({step.label: step.number});
        }
        break;
    }

    if (boxContents.isEmpty) {
      boxes.remove(step.numBox);
    } else {
      boxes[step.numBox] = boxContents;
    }
  }

  return boxes;
}

String toStringBoxes(Map<int, List<Map<String, int>>> boxes) {
  String res = '';

  for (int iBox = 0; iBox < hashTableSize; iBox++) {
    if (boxes.containsKey(iBox)) {
      res += 'Box $iBox : ';
      for (Map<String, int> content in boxes[iBox]!) {
        for (MapEntry<String, int> entry in content.entries) {
          res += '[${entry.key} ${entry.value}] ';
        }
      }
      res += '\n';
    }
  }

  return res;
}

int calculatePower(Map<int, List<Map<String, int>>> boxes) {
  int res = 0;

  for (int iBox = 0; iBox < hashTableSize; iBox++) {
    if (boxes.containsKey(iBox)) {
      for (int iSlot = 0; iSlot < boxes[iBox]!.length; iSlot++) {
        for (int value in boxes[iBox]![iSlot].values) {
          res += (iBox + 1) * (iSlot + 1) * value;
        }
      }
    }
  }

  return res;
}

int solve(List<String> input) {
  List<String> stepsStr = input[0].split(',');
  Map<int, List<Map<String, int>>> boxes = getBoxes(stepsStr);
  return calculatePower(boxes);
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}