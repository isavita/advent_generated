
import 'dart:io';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final valves = parseInput(input);
  final distances = floydWarshall(valves);
  final maxPressure = findMaxPressure(valves, distances, 30);
  print('Part 1: $maxPressure');
  final maxPressureWithElephant = findMaxPressureWithElephant(valves, distances, 26);
  print('Part 2: $maxPressureWithElephant');
}

class Valve {
  final String name;
  final int flowRate;
  final List<String> tunnels;

  Valve(this.name, this.flowRate, this.tunnels);
}

Map<String, Valve> parseInput(List<String> input) {
  final valves = <String, Valve>{};
  for (final line in input) {
    final parts = line.split(' ');
    final name = parts[1];
    final flowRate = int.parse(parts[4].split('=')[1].replaceAll(';', ''));
    final tunnels = parts.sublist(9).map((e) => e.replaceAll(',', '')).toList();
    valves[name] = Valve(name, flowRate, tunnels);
  }
  return valves;
}

Map<String, Map<String, int>> floydWarshall(Map<String, Valve> valves) {
  final distances = <String, Map<String, int>>{};
  for (final valve in valves.keys) {
    distances[valve] = {};
    for (final other in valves.keys) {
      distances[valve]![other] = valve == other ? 0 : 1000000;
    }
  }
  for (final valve in valves.values) {
    for (final tunnel in valve.tunnels) {
      distances[valve.name]![tunnel] = 1;
    }
  }
  for (final k in valves.keys) {
    for (final i in valves.keys) {
      for (final j in valves.keys) {
        if (distances[i]![j]! > distances[i]![k]! + distances[k]![j]!) {
          distances[i]![j] = distances[i]![k]! + distances[k]![j]!;
        }
      }
    }
  }
  return distances;
}

int findMaxPressure(
    Map<String, Valve> valves, Map<String, Map<String, int>> distances, int timeLimit) {
  final usefulValves = valves.values.where((v) => v.flowRate > 0).map((v) => v.name).toSet();
  final cache = <String, int>{};

  int dfs(String current, int time, Set<String> openValves) {
    final state = '$current,$time,${openValves.toList()..sort()}';
    if (cache.containsKey(state)) return cache[state]!;

    var maxPressure = 0;
    for (final next in usefulValves) {
      if (!openValves.contains(next)) {
        final travelTime = distances[current]![next]! + 1;
        final remainingTime = time - travelTime;
        if (remainingTime > 0) {
          final newOpenValves = Set<String>.from(openValves)..add(next);
          final pressure = valves[next]!.flowRate * remainingTime +
              dfs(next, remainingTime, newOpenValves);
          maxPressure = pressure > maxPressure ? pressure : maxPressure;
        }
      }
    }
    cache[state] = maxPressure;
    return maxPressure;
  }

  return dfs('AA', timeLimit, {});
}

int findMaxPressureWithElephant(
    Map<String, Valve> valves, Map<String, Map<String, int>> distances, int timeLimit) {
  final usefulValves = valves.values.where((v) => v.flowRate > 0).map((v) => v.name).toList();
  var maxPressure = 0;

  for (var i = 0; i < (1 << usefulValves.length); i++) {
    final myValves = <String>{};
    final elephantValves = <String>{};
    for (var j = 0; j < usefulValves.length; j++) {
      if ((i >> j) & 1 == 1) {
        myValves.add(usefulValves[j]);
      } else {
        elephantValves.add(usefulValves[j]);
      }
    }

    final myPressure = findMaxPressureForSubset(valves, distances, timeLimit, myValves);
    final elephantPressure =
        findMaxPressureForSubset(valves, distances, timeLimit, elephantValves);
    maxPressure =
        myPressure + elephantPressure > maxPressure ? myPressure + elephantPressure : maxPressure;
  }

  return maxPressure;
}

int findMaxPressureForSubset(Map<String, Valve> valves, Map<String, Map<String, int>> distances,
    int timeLimit, Set<String> subset) {
  final cache = <String, int>{};

  int dfs(String current, int time, Set<String> openValves) {
    final state = '$current,$time,${openValves.toList()..sort()}';
    if (cache.containsKey(state)) return cache[state]!;

    var maxPressure = 0;
    for (final next in subset) {
      if (!openValves.contains(next)) {
        final travelTime = distances[current]![next]! + 1;
        final remainingTime = time - travelTime;
        if (remainingTime > 0) {
          final newOpenValves = Set<String>.from(openValves)..add(next);
          final pressure = valves[next]!.flowRate * remainingTime +
              dfs(next, remainingTime, newOpenValves);
          maxPressure = pressure > maxPressure ? pressure : maxPressure;
        }
      }
    }
    cache[state] = maxPressure;
    return maxPressure;
  }

  return dfs('AA', timeLimit, {});
}
