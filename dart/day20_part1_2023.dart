import 'dart:io';

class Module {
  String name;
  int prefix;
  List<String> destinations;
  bool state;
  Map<String, PulseValue> memory;

  Module(this.name, this.prefix, this.destinations, this.state, this.memory);
}

enum PulseValue { Low, High }

class Pulse {
  PulseValue value;
  String fromName;
  String toName;

  Pulse({required this.value, required this.fromName, required this.toName});
}

const int FlipFlop = 37;
const int Conjunction = 38;

Map<String, Module> parseInput(List<String> input) {
  List<int> prefixes = [FlipFlop, Conjunction];
  Map<String, Module> modules = {};

  for (String line in input) {
    List<String> parts = line.split(' -> ');

    Module module = Module('', 0, [], false, {});
    bool isPrefix = false;
    for (int prefix in prefixes) {
      if (parts[0].codeUnitAt(0) == prefix) {
        module.prefix = prefix;
        module.name = parts[0].substring(1);
        isPrefix = true;
        continue;
      }
    }
    if (!isPrefix) {
      module.name = parts[0];
    }
    module.destinations = parts[1].split(', ');
    module.memory = {};

    modules[module.name] = module;
  }

  for (Module module in modules.values) {
    for (String destName in module.destinations) {
      if (modules.containsKey(destName) && modules[destName]!.prefix == Conjunction) {
        modules[destName]!.memory[module.name] = PulseValue.Low;
      }
    }
  }

  return modules;
}

List<int> pushButton(Map<String, Module> modules, Pulse startPulse, int numCycle) {
  int cntLow = 0;
  int cntHigh = 0;
  List<Pulse> pulseQueue = [];

  for (int i = 0; i < numCycle; i++) {
    pulseQueue.add(startPulse);

    while (pulseQueue.isNotEmpty) {
      Pulse pulse = pulseQueue.removeAt(0);

      if (pulse.value == PulseValue.Low) {
        cntLow++;
      } else {
        cntHigh++;
      }

      if (!modules.containsKey(pulse.toName)) {
        continue;
      }

      Module module = modules[pulse.toName]!;
      PulseValue newPulseValue;
      switch (module.prefix) {
        case FlipFlop:
          if (pulse.value == PulseValue.Low) {
            module.state = !module.state;
            if (module.state) {
              newPulseValue = PulseValue.High;
            } else {
              newPulseValue = PulseValue.Low;
            }
          } else {
            continue;
          }
          break;

        case Conjunction:
          module.memory[pulse.fromName] = pulse.value;
          bool isHighForAll = true;
          for (PulseValue value in module.memory.values) {
            if (value == PulseValue.Low) {
              isHighForAll = false;
              break;
            }
          }

          if (isHighForAll) {
            newPulseValue = PulseValue.Low;
          } else {
            newPulseValue = PulseValue.High;
          }
          break;

        default:
          newPulseValue = pulse.value;
      }

      for (String destName in module.destinations) {
        Pulse newPulse = Pulse(
          value: newPulseValue,
          fromName: pulse.toName,
          toName: destName,
        );
        pulseQueue.add(newPulse);
      }
    }
  }

  return [cntLow, cntHigh];
}

int solve(List<String> input) {
  Pulse startPulse = Pulse(
    value: PulseValue.Low,
    fromName: 'button',
    toName: 'broadcaster',
  );
  int numCycle = 1000;

  Map<String, Module> modules = parseInput(input);

  List<int> counts = pushButton(modules, startPulse, numCycle);

  return counts[0] * counts[1];
}

List<String> readFile(String fileName) {
  final file = File(fileName);
  List<String> lines = file.readAsLinesSync();
  return lines;
}

void main() {
  List<String> input = readFile("input.txt");
  print(solve(input));
}