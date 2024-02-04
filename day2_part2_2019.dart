import 'dart:io';

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();
  var original = lines[0].split(',').map(int.parse).toList();

  for (var noun = 0; noun <= 99; noun++) {
    for (var verb = 0; verb <= 99; verb++) {
      var memory = List<int>.from(original);
      memory[1] = noun;
      memory[2] = verb;
      if (execute(memory) == 19690720) {
        print(100 * noun + verb);
        return;
      }
    }
  }
}

int execute(List<int> memory) {
  for (var i = 0; i < memory.length; i += 4) {
    switch (memory[i]) {
      case 1:
        memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]];
        break;
      case 2:
        memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]];
        break;
      case 99:
        return memory[0];
    }
  }
  return memory[0];
}