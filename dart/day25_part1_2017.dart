import 'dart:io';
import 'dart:convert';
import 'dart:core';

void main() {
  final result = runTuringMachine('input.txt');
  print(result);
}

int runTuringMachine(String filePath) {
  final input = parseInput(filePath);
  final initialState = input[0];
  final steps = input[1];
  final states = input[2];

  var tape = <int, int>{};
  var cursor = 0;
  var currentState = initialState;
  var checksum = 0;

  for (var i = 0; i < steps; i++) {
    final value = tape[cursor] ?? 0;
    final newValue = states[currentState]![value]![0] as int;
    final move = states[currentState]![value]![1] as int;
    final nextState = states[currentState]![value]![2] as String;

    tape[cursor] = newValue;
    cursor += move;
    currentState = nextState;
  }

  for (final value in tape.values) {
    checksum += value;
  }

  return checksum;
}

List parseInput(String filePath) {
  final lines = File(filePath).readAsLinesSync();
  final initialState = lines[0][lines[0].length - 2];
  final steps = int.parse(RegExp(r'\d+').firstMatch(lines[1])!.group(0)!);

  final states = <String, Map<int, List>>{};
  for (var i = 3; i < lines.length; i += 10) {
    final state = lines[i][lines[i].length - 2];
    final value0 = int.parse(lines[i + 2][lines[i + 2].length - 2]);
    final move0 = lines[i + 3].endsWith('left.') ? -1 : 1;
    final nextState0 = lines[i + 4][lines[i + 4].length - 2];
    final value1 = int.parse(lines[i + 6][lines[i + 6].length - 2]);
    final move1 = lines[i + 7].endsWith('left.') ? -1 : 1;
    final nextState1 = lines[i + 8][lines[i + 8].length - 2];
    states[state] = {0: [value0, move0, nextState0], 1: [value1, move1, nextState1]};
  }

  return [initialState, steps, states];
}