import 'dart:io';

class Network {
  String instructions;
  Map<String, List<String>> nodes;

  Network({required this.instructions, required this.nodes});
}

Network parseInput(List<String> input) {
  String instructions = input[0];

  Map<String, List<String>> nodes = {};
  for (int i = 2; i < input.length; i++) {
    List<String> parsedLine = parseLine(input[i]);
    nodes[parsedLine[0]] = [parsedLine[1], parsedLine[2]];
  }

  return Network(instructions: instructions, nodes: nodes);
}

List<String> parseLine(String line) {
  List<String> parts = line.split(' = ');
  String head = parts[0];
  List<String> childrenParts = parts[1].replaceAll('(', '').replaceAll(')', '').split(', ');
  String child1 = childrenParts[0];
  String child2 = childrenParts[1];

  return [head, child1, child2];
}

int gcd(int a, int b) {
  while (b != 0) {
    int temp = b;
    b = a % b;
    a = temp;
  }
  return a;
}

int lcm(int a, int b) {
  return (a * b) ~/ gcd(a, b);
}

int lcmSlice(List<int> nums) {
  if (nums.isEmpty) {
    return 0;
  }

  int res = nums[0];
  for (int i = 1; i < nums.length; i++) {
    res = (res * nums[i]) ~/ gcd(res, nums[i]);
  }

  return res;
}

int solve(List<String> input) {
  Network network = parseInput(input);

  List<String> starts = [];
  network.nodes.keys.forEach((node) {
    if (node[node.length - 1] == 'A') {
      starts.add(node);
    }
  });

  List<int> steps = List.filled(starts.length, 0);
  int instructionsLength = network.instructions.length;
  for (int i = 0; i < starts.length; i++) {
    String element = starts[i];
    while (element[element.length - 1] != 'Z') {
      int instructionIndex = steps[i] % instructionsLength;
      String instruction = network.instructions[instructionIndex];
      if (instruction == 'L') {
        element = network.nodes[element]![0];
      } else {
        element = network.nodes[element]![1];
      }
      steps[i]++;
    }
  }

  int res = lcmSlice(steps);
  return res;
}

List<String> readFile(String fileName) {
  File file = File(fileName);
  List<String> lines = file.readAsLinesSync();
  return lines;
}

void main() {
  List<String> input = readFile('input.txt');
  print(solve(input));
}
