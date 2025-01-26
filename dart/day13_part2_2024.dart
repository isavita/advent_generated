
import 'dart:io';
import 'dart:convert';

class Machine {
  int ax, ay, bx, by, px, py;

  Machine(this.ax, this.ay, this.bx, this.by, this.px, this.py);
}

void main() {
  const offset = 10000000000000;
  List<Machine> machines = readInput("input.txt");
  for (var m in machines) {
    m.px += offset;
    m.py += offset;
  }

  List<int> results = [];
  for (var m in machines) {
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
  for (var line in fileLines) {
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
  for (var l in lines) {
    l = l.replaceAll("Button A:", "A:").replaceAll("Button B:", "B:").replaceAll("Prize:", "P:");
    if (l.startsWith("A:")) {
      var coords = parseLine(l.substring(2));
      ax = coords[0];
      ay = coords[1];
    } else if (l.startsWith("B:")) {
      var coords = parseLine(l.substring(2));
      bx = coords[0];
      by = coords[1];
    } else if (l.startsWith("P:")) {
      var coords = parsePrize(l.substring(2));
      px = coords[0];
      py = coords[1];
    }
  }
  return Machine(ax, ay, bx, by, px, py);
}

List<int> parseLine(String s) {
  List<String> parts = s.trim().split(",");
  int x = parseVal(parts[0]);
  int y = parseVal(parts[1]);
  return [x, y];
}

List<int> parsePrize(String s) {
  List<String> parts = s.trim().split(",");
  int x = parseValPrize(parts[0]);
  int y = parseValPrize(parts[1]);
  return [x, y];
}

int parseVal(String s) {
  s = s.trim();
  s = s.replaceAll("X+", "").replaceAll("Y+", "").replaceAll("X=", "").replaceAll("Y=", "");
  return int.parse(s);
}

int parseValPrize(String s) {
  s = s.trim();
  s = s.replaceAll("X=", "").replaceAll("Y=", "");
  return int.parse(s);
}

int solveMachine(Machine m) {
  int D = m.ax * m.by - m.ay * m.bx;
  if (D == 0) {
    return -1;
  }
  int numA = m.px * m.by - m.py * m.bx;
  int numB = -m.px * m.ay + m.py * m.ax;
  if (numA % D != 0 || numB % D != 0) {
    return -1;
  }
  int a = numA ~/ D;
  int b = numB ~/ D;
  if (a < 0 || b < 0) {
    return -1;
  }
  return 3 * a + b;
}
