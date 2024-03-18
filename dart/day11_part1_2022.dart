import 'dart:io';
import 'dart:math';

class Monkey {
  List<int> items = [];
  late Function(int) operation;
  late int div;
  late List<int> next;
  int inspections = 0;

  Monkey.parse(String s) {
    var lines = s.split('\n');
    items = lines[1].split(': ')[1].split(', ').map(int.parse).toList();
    var f = lines[2].split('= ')[1].split(' ');
    switch (f[1]) {
      case '+':
        if (f[2] == 'old') {
          operation = (old) => old + old;
        } else {
          operation = (old) => old + int.parse(f[2]);
        }
        break;
      case '*':
        if (f[2] == 'old') {
          operation = (old) => old * old;
        } else {
          operation = (old) => old * int.parse(f[2]);
        }
        break;
    }
    div = int.parse(lines[3].split(' ')[5]);
    next = [int.parse(lines[4].split(' ')[9]), int.parse(lines[5].split(' ')[9])];
  }
}

int monkeyBusiness(List<Monkey> monkeys, int rounds, bool worry) {
  int div = 1;
  for (var m in monkeys) {
    div *= m.div;
  }

  for (int i = 0; i < rounds; i++) {
    for (var m in monkeys) {
      while (m.items.isNotEmpty) {
        m.inspections++;
        int item = m.operation(m.items.first);
        if (worry) {
          item %= div;
        } else {
          item ~/= 3;
        }
        if (item % m.div == 0) {
          monkeys[m.next[0]].items.add(item);
        } else {
          monkeys[m.next[1]].items.add(item);
        }
        m.items.removeAt(0);
      }
    }
  }

  var inspections = monkeys.map((m) => m.inspections).toList();
  inspections.sort((a, b) => b.compareTo(a));
  return inspections[0] * inspections[1];
}

void main() {
  var monkeys = <Monkey>[];
  var input = File('input.txt').readAsStringSync();
  for (var m in input.split('\n\n')) {
    monkeys.add(Monkey.parse(m));
  }
  print(monkeyBusiness(monkeys, 20, false));
}