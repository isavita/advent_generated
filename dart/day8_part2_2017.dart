
import 'dart:io';

void main() {
  var registers = {};
  var highestValue = 0;

  List<String> lines = File('input.txt').readAsLinesSync();

  for (var line in lines) {
    var parts = line.split(' ');
    var register = parts[0];
    var operation = parts[1];
    var amount = int.parse(parts[2]);
    var conditionRegister = parts[4];
    var conditionOperator = parts[5];
    var conditionValue = int.parse(parts[6]);

    registers.putIfAbsent(register, () => 0);
    registers.putIfAbsent(conditionRegister, () => 0);

    if (conditionOperator == '>' && registers[conditionRegister] > conditionValue ||
        conditionOperator == '<' && registers[conditionRegister] < conditionValue ||
        conditionOperator == '>=' && registers[conditionRegister] >= conditionValue ||
        conditionOperator == '<=' && registers[conditionRegister] <= conditionValue ||
        conditionOperator == '==' && registers[conditionRegister] == conditionValue ||
        conditionOperator == '!=' && registers[conditionRegister] != conditionValue) {
      if (operation == 'inc') {
        registers[register] += amount;
      } else {
        registers[register] -= amount;
      }

      if (registers[register] > highestValue) {
        highestValue = registers[register];
      }
    }
  }

  var largestValue = registers.values.reduce((value, element) => value > element ? value : element);

  print(largestValue);
  print(highestValue);
}
