
import 'dart:io';
import 'dart:convert';

int evaluatePart1(List<int> nums, List<String> operators) {
  if (nums.isEmpty) {
    return 0;
  }
  int result = nums[0];
  for (int i = 0; i < operators.length; ++i) {
    String op = operators[i];
    int num = nums[i + 1];
    if (op == '+') {
      result += num;
    } else if (op == '*') {
      result *= num;
    }
  }
  return result;
}

bool isEquationTruePart1(int target, List<int> nums) {
  if (nums.length <= 1) {
    return nums.length == 1 && nums[0] == target;
  }
  List<List<String>> operatorCombinations = generateOperatorCombinationsPart1(nums.length - 1);
  for (var operators in operatorCombinations) {
    if (evaluatePart1(nums, operators) == target) {
      return true;
    }
  }
  return false;
}

List<List<String>> generateOperatorCombinationsPart1(int numOperators) {
  if (numOperators == 0) {
    return [[]];
  }
  List<List<String>> previousCombinations = generateOperatorCombinationsPart1(numOperators - 1);
  List<List<String>> currentCombinations = [];
  for (var combination in previousCombinations) {
    currentCombinations.add(combination + ['+']);
    currentCombinations.add(combination + ['*']);
  }
  return currentCombinations;
}

int evaluatePart2(List<int> nums, List<String> operators) {
  if (nums.isEmpty) {
    return 0;
  }
  int result = nums[0];
  for (int i = 0; i < operators.length; ++i) {
    String op = operators[i];
    int num = nums[i + 1];
    if (op == '+') {
      result += num;
    } else if (op == '*') {
      result *= num;
    } else if (op == '||') {
      result = int.parse('$result$num');
    }
  }
  return result;
}

bool isEquationTruePart2(int target, List<int> nums) {
  if (nums.length <= 1) {
    return nums.length == 1 && nums[0] == target;
  }
  List<List<String>> operatorCombinations = generateOperatorCombinationsPart2(nums.length - 1);
  for (var operators in operatorCombinations) {
    if (evaluatePart2(nums, operators) == target) {
      return true;
    }
  }
  return false;
}

List<List<String>> generateOperatorCombinationsPart2(int numOperators) {
  if (numOperators == 0) {
    return [[]];
  }
  List<List<String>> previousCombinations = generateOperatorCombinationsPart2(numOperators - 1);
  List<List<String>> currentCombinations = [];
  for (var combination in previousCombinations) {
    currentCombinations.add(combination + ['+']);
    currentCombinations.add(combination + ['*']);
    currentCombinations.add(combination + ['||']);
  }
  return currentCombinations;
}

void main() {
  File inputFile = File('input.txt');
  int totalCalibrationResultPart1 = 0;
  int totalCalibrationResultPart2 = 0;

  List<String> lines = inputFile.readAsLinesSync();
  for (String line in lines) {
    List<String> parts = line.split(': ');
    int targetValue = int.parse(parts[0]);
    List<int> numbers = parts[1].split(' ').map(int.parse).toList();

    if (isEquationTruePart1(targetValue, numbers)) {
      totalCalibrationResultPart1 += targetValue;
    }
    if (isEquationTruePart2(targetValue, numbers)) {
      totalCalibrationResultPart2 += targetValue;
    }
  }

  print(totalCalibrationResultPart2);
}
