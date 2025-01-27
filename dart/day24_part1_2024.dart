
import 'dart:io';

class Gate {
  String input1;
  String input2;
  String operation;
  String output;
  Gate(this.input1, this.input2, this.operation, this.output);
}

void main() {
  var file = File('input.txt');
  if (!file.existsSync()) {
    print('Error opening input.txt');
    return;
  }
  var lines = file.readAsLinesSync();
  var wires = <String, int>{};
  var gates = <Gate>[];
  var parsingWires = true;
  for (var line in lines) {
    line = line.trim();
    if (line.isEmpty) {
      parsingWires = false;
      continue;
    }
    if (parsingWires) {
      var parts = line.split(':');
      if (parts.length != 2) {
        print('Invalid wire definition: $line');
        return;
      }
      var wireName = parts[0].trim();
      var wireValue = int.tryParse(parts[1].trim());
      if (wireValue == null) {
        print('Invalid wire value in line: $line');
        return;
      }
      wires[wireName] = wireValue;
    } else {
      var parts = line.split(RegExp(r'\s+'));
      if (parts.length != 5 || parts[3] != '->') {
        print('Invalid gate definition: $line');
        return;
      }
      gates.add(Gate(parts[0], parts[2], parts[1], parts[4]));
    }
  }
  var remainingGates = List<Gate>.from(gates);
  while (remainingGates.isNotEmpty) {
    var progress = false;
    var newRemainingGates = <Gate>[];
    for (var gate in remainingGates) {
      var val1 = wires[gate.input1];
      var val2 = wires[gate.input2];
      if (val1 != null && val2 != null) {
        int outputVal;
        if (gate.operation == 'AND') {
          outputVal = (val1 == 1 && val2 == 1) ? 1 : 0;
        } else if (gate.operation == 'OR') {
          outputVal = (val1 == 1 || val2 == 1) ? 1 : 0;
        } else if (gate.operation == 'XOR') {
          outputVal = (val1 != val2) ? 1 : 0;
        } else {
          print('Unknown operation: ${gate.operation}');
          return;
        }
        wires[gate.output] = outputVal;
        progress = true;
      } else {
        newRemainingGates.add(gate);
      }
    }
    if (!progress) {
      print('Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.');
      return;
    }
    remainingGates = newRemainingGates;
  }
  var zWires = <int, int>{};
  var zRegex = RegExp(r'^z(\d+)$');
  for (var wire in wires.keys) {
    var match = zRegex.firstMatch(wire);
    if (match != null) {
      var index = int.parse(match.group(1)!);
      zWires[index] = wires[wire]!;
    }
  }
  if (zWires.isEmpty) {
    print("No wires starting with 'z' found.");
    return;
  }
  var indices = zWires.keys.toList()..sort();
  var binaryBits = List<int>.filled(indices.length, 0);
  for (var i = 0; i < indices.length; i++) {
    binaryBits[i] = zWires[indices[i]]!;
  }
  binaryBits = binaryBits.reversed.toList();
  var binaryString = binaryBits.join();
  var decimalValue = int.parse(binaryString, radix: 2);
  print(decimalValue);
}
