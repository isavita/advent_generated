import 'dart:io';

void main() {
  var file = new File('input.txt');
  var lines = file.readAsLinesSync();

  var initialState = '';
  var rules = new Map<String, String>();

  for (var line in lines) {
    if (line.contains('initial state')) {
      initialState = line.split(': ')[1];
    } else if (line.contains('=>')) {
      var parts = line.split(' => ');
      rules[parts[0]] = parts[1][0];
    }
  }

  var state = new Map<int, String>();
  for (var i = 0; i < initialState.length; i++) {
    if (initialState[i] == '#') {
      state[i] = '#';
    }
  }

  for (var generation = 0; generation < 20; generation++) {
    var newState = new Map<int, String>();
    var minPot = state.keys.reduce((a, b) => a < b ? a : b) - 2;
    var maxPot = state.keys.reduce((a, b) => a > b ? a : b) + 2;
    for (var i = minPot; i <= maxPot; i++) {
      var pattern = '';
      for (var j = i - 2; j <= i + 2; j++) {
        pattern += state[j] == '#' ? '#' : '.';
      }
      if (rules[pattern] == '#') {
        newState[i] = '#';
      }
    }
    state = newState;
  }

  var sum = state.keys.reduce((a, b) => a + b);

  print(sum);
}