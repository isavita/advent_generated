
import 'dart:io';

enum ModuleType { broadcaster, flipFlop, conjunction }

class Module {
  String name;
  ModuleType type;
  List<String> connectsTo;
  Module(this.name, this.type, this.connectsTo);
}

class FlipFlop extends Module {
  bool state;
  FlipFlop(String name, List<String> connectsTo, this.state) : super(name, ModuleType.flipFlop, connectsTo);
}

class Conjunction extends Module {
  Map<String, bool> watches;
  Conjunction(String name, List<String> connectsTo, this.watches) : super(name, ModuleType.conjunction, connectsTo);
}

class Broadcaster extends Module {
  Broadcaster(String name, List<String> connectsTo) : super(name, ModuleType.broadcaster, connectsTo);
}

class State {
  String from;
  String name;
  bool pulse;
  State(this.from, this.name, this.pulse);
}

void main() async {
  final file = File('input.txt');
  final lines = await file.readAsLines();
  final connections = <String, Module>{};

  for (final line in lines) {
    if (line.contains('broadcaster')) {
      final parts = line.split(' -> ');
      final name = parts[0];
      final connectsTo = parts[1].split(', ');
      connections[name] = Broadcaster(name, connectsTo);
    } else if (line.contains('%')) {
      final parts = line.split(' -> ');
      final name = parts[0].substring(1);
      final connectsTo = parts[1].split(', ');
      connections[name] = FlipFlop(name, connectsTo, false);
    } else {
      final parts = line.split(' -> ');
      final name = parts[0].substring(1);
      final connectsTo = parts[1].split(', ');
      final watches = <String, bool>{};
      connections[name] = Conjunction(name, connectsTo, watches);
    }
  }

  for (final module in connections.values) {
    if (module is Conjunction) {
      for (final otherModule in connections.values) {
        if (otherModule.connectsTo.contains(module.name)) {
          module.watches[otherModule.name] = false;
        }
      }
    }
  }

  final loopLengths = <String, int>{};
  final pxPrev = connections.entries.firstWhere((entry) => entry.value.connectsTo.contains('rx')).key;
  final conj = connections[pxPrev] as Conjunction;
  for (final name in conj.watches.keys) {
    loopLengths[name] = -1;
  }

  int pressNumber = 0;
  while (true) {
    pressNumber++;
    final pulses = simulatePress(connections, loopLengths, pressNumber);
    if (pulses) break;
    if (loopLengths.values.every((length) => length != -1)) break;
  }

  int sum = 1;
  for (final length in loopLengths.values) {
    sum *= length;
  }

  print(sum);
}

bool simulatePress(Map<String, Module> connections, Map<String, int> loops, int pressNumber) {
  final queue = <State>[State('button', 'broadcaster', false)];
  final pulses = [1, 0];

  while (queue.isNotEmpty) {
    final currentState = queue.removeAt(0);
    final module = connections[currentState.name];

    if (currentState.name == 'out') continue;
    if (currentState.name == 'rx' && !currentState.pulse) return true;

    final pulse = currentState.pulse;

    switch (module.runtimeType) {
      case Broadcaster:
        final broadcaster = module as Broadcaster;
        for (final name in broadcaster.connectsTo) {
          queue.add(State(module.name, name, pulse));
          pulses[pulse ? 1 : 0]++;
        }
        break;
      case FlipFlop:
        final flipFlop = module as FlipFlop;
        if (!pulse) {
          flipFlop.state = !flipFlop.state;
          for (final name in flipFlop.connectsTo) {
            queue.add(State(module.name, name, flipFlop.state));
            pulses[flipFlop.state ? 1 : 0]++;
          }
        }
        break;
      case Conjunction:
        final conjunction = module as Conjunction;
        conjunction.watches[currentState.from] = pulse;

        final allTrue = conjunction.watches.values.every((state) => state);
        for (final name in conjunction.connectsTo) {
          queue.add(State(module.name, name, !allTrue));
          pulses[allTrue ? 0 : 1]++;
        }

        final currLoop = loops[conjunction.name];
        if (currLoop != null && !allTrue && currLoop == -1) {
          loops[conjunction.name] = pressNumber;
        }
        break;
    }
  }
  return false;
}
