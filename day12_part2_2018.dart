import 'dart:io';

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var initialState = '';
  var rules = Map<String, String>();

  for (var line in lines) {
    if (line.contains('initial state')) {
      initialState = line.split(': ')[1];
    } else if (line.contains('=>')) {
      var parts = line.split(' => ');
      rules[parts[0]] = parts[1][0];
    }
  }

  var state = Map<int, String>();
  for (var i = 0; i < initialState.length; i++) {
    if (initialState[i] == '#') {
      state[i] = '#';
    }
  }

  var previousPattern = '';
  var previousSum = 0;
  var offset = 0;
  for (var generation = 0; generation < 50000000000; generation++) {
    var newState = Map<int, String>();
    var minMax = minMaxKeys(state);
    var minPot = minMax[0];
    var maxPot = minMax[1];
    for (var i = minPot - 2; i <= maxPot + 2; i++) {
      var pattern = '';
      for (var j = i - 2; j <= i + 2; j++) {
        if (state.containsKey(j) && state[j] == '#') {
          pattern += '#';
        } else {
          pattern += '.';
        }
      }
      if (rules[pattern] == '#') {
        newState[i] = '#';
      }
    }
    state = newState;

    var currentPatternSum = statePattern(state);
    var currentPattern = currentPatternSum[0];
    var currentSum = currentPatternSum[1];
    if (currentPattern == previousPattern) {
      offset = currentSum - previousSum;
      var remainingGenerations = 50000000000 - generation - 1;
      var finalSum = currentSum + offset * remainingGenerations;
      print(finalSum);
      return;
    }
    previousPattern = currentPattern;
    previousSum = currentSum;
  }
}

List<int> minMaxKeys(Map<int, String> m) {
  var minKey = m.keys.reduce((a, b) => a < b ? a : b);
  var maxKey = m.keys.reduce((a, b) => a > b ? a : b);
  return [minKey, maxKey];
}

List<dynamic> statePattern(Map<int, String> m) {
  var minMax = minMaxKeys(m);
  var minPot = minMax[0];
  var maxPot = minMax[1];
  var pattern = '';
  var sum = 0;
  for (var i = minPot; i <= maxPot; i++) {
    if (m.containsKey(i) && m[i] == '#') {
      pattern += '#';
      sum += i;
    } else {
      pattern += '.';
    }
  }
  return [pattern, sum];
}