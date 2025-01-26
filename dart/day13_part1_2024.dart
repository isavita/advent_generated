
import 'dart:io';

class Machine {
  int ax, ay, bx, by, px, py;

  Machine(this.ax, this.ay, this.bx, this.by, this.px, this.py);
}

void main() {
  List<Machine> machines = readInput("input.txt");
  List<int> results = [];

  for (Machine m in machines) {
    int cost = solveMachine(m);
    if (cost >= 0) {
      results.add(cost);
    }
  }

  if (results.isEmpty) {
    print("0 0");
    return;
  }

  int count = results.length;
  int sum = results.reduce((a, b) => a + b);
  print("$count $sum");
}

List<Machine> readInput(String filename) {
  List<Machine> machines = [];
  List<String> lines = [];
  File file = File(filename);
  List<String> fileLines = file.readAsLinesSync();

  for (String line in fileLines) {
    line = line.trim();
    if (line.isEmpty) {
      if (lines.isNotEmpty) {
        machines.add(parseMachine(lines));
        lines.clear();
      }
    } else {
      lines.add(line);
    }
  }

  if (lines.isNotEmpty) {
    machines.add(parseMachine(lines));
  }

  return machines;
}

Machine parseMachine(List<String> lines) {
  int ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0;

  for (String line in lines) {
    line = line.replaceAll("Button A:", "A:").replaceAll("Button B:", "B:").replaceAll("Prize:", "P:");
    if (line.startsWith("A:")) {
      List<int> coords = parseLine(line.substring(2));
      ax = coords[0];
      ay = coords[1];
    } else if (line.startsWith("B:")) {
      List<int> coords = parseLine(line.substring(2));
      bx = coords[0];
      by = coords[1];
    } else if (line.startsWith("P:")) {
      List<int> coords = parsePrize(line.substring(2));
      px = coords[0];
      py = coords[1];
    }
  }

  return Machine(ax, ay, bx, by, px, py);
}

List<int> parseLine(String s) {
  List<String> parts = s.trim().split(",");
  int x = parseVal(parts[0].trim());
  int y = parseVal(parts[1].trim());
  return [x, y];
}

List<int> parsePrize(String s) {
  List<String> parts = s.trim().split(",");
  int x = parseValPrize(parts[0].trim());
  int y = parseValPrize(parts[1].trim());
  return [x, y];
}

int parseVal(String s) {
  s = s.replaceAll("X+", "").replaceAll("Y+", "").replaceAll("X=", "").replaceAll("Y=", "");
  return int.parse(s);
}

int parseValPrize(String s) {
  s = s.replaceAll("X=", "").replaceAll("Y=", "");
  return int.parse(s);
}

int solveMachine(Machine m) {
  int minCost = -1;

  for (int aCount = 0; aCount <= 100; aCount++) {
    for (int bCount = 0; bCount <= 100; bCount++) {
      int x = m.ax * aCount + m.bx * bCount;
      int y = m.ay * aCount + m.by * bCount;
      if (x == m.px && y == m.py) {
        int cost = aCount * 3 + bCount;
        if (minCost < 0 || cost < minCost) {
          minCost = cost;
        }
      }
    }
  }

  return minCost;
}
