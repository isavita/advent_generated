
import 'dart:io';
import 'dart:convert';

void main() async {
  String input = await File('input.txt').readAsString();
  print(rtgHellDay(input));
}

int rtgHellDay(String input) {
  State currentState = newInitialState(input);
  currentState.floors[0].addAll([
    Halves(false, "elerium"),
    Halves(true, "elerium"),
    Halves(false, "dilithium"),
    Halves(true, "dilithium"),
  ]);

  var queue = [currentState];
  var prevStates = <String, bool>{};

  while (queue.isNotEmpty) {
    State front = queue.removeAt(0);
    if (front.isDone()) return front.steps;

    String hash = front.hashKey();
    if (prevStates[hash] != null) continue;
    prevStates[hash] = true;

    queue.addAll(front.getNextStates());
  }
  return -1;
}

class Halves {
  final bool isChip;
  final String material;

  Halves(this.isChip, this.material);

  @override
  String toString() {
    return '${material} ${isChip ? "microchip" : "generator"}';
  }
}

class State {
  final List<List<Halves>> floors = List.generate(4, (_) => []);
  int elevatorLevel = 0;
  int steps = 0;

  State clone() {
    State clone = State();
    clone.elevatorLevel = elevatorLevel;
    clone.steps = steps;
    for (int i = 0; i < floors.length; i++) {
      clone.floors[i].addAll(floors[i]);
    }
    return clone;
  }

  bool isValid() {
    for (var floor in floors) {
      var gensSeen = <String>{};
      for (var half in floor) {
        if (!half.isChip) gensSeen.add(half.material);
      }
      if (gensSeen.isNotEmpty) {
        for (var half in floor) {
          if (half.isChip && !gensSeen.contains(half.material)) return false;
        }
      }
    }
    return true;
  }

  bool isDone() {
    return floors.take(3).expand((x) => x).isEmpty;
  }

  String hashKey() {
    var genChipPairs = <List<int>>[];
    var mapGenToIndex = <String, int>{};
    var mapChipToIndex = <String, int>{};

    for (int flIndex = 0; flIndex < floors.length; flIndex++) {
      for (var half in floors[flIndex]) {
        if (half.isChip) {
          mapChipToIndex[half.material] = flIndex;
        } else {
          mapGenToIndex[half.material] = flIndex;
        }
      }
    }

    for (var material in mapGenToIndex.keys) {
      genChipPairs.add([mapGenToIndex[material]!, mapChipToIndex[material]!]);
    }

    genChipPairs.sort((a, b) {
      if (a[0] != b[0]) return a[0].compareTo(b[0]);
      return a[1].compareTo(b[1]);
    });

    return '${elevatorLevel}${genChipPairs}';
  }

  List<State> getNextStates() {
    var futureStates = <State>[];
    var movablePermIndices = getMovablePermIndices();
    var eleDiffs = <int>[];

    if (elevatorLevel < floors.length - 1) eleDiffs.add(1);
    if (elevatorLevel > 0) eleDiffs.add(-1);

    for (var eleDiff in eleDiffs) {
      for (var permIndices in movablePermIndices) {
        var cl = clone();
        cl.elevatorLevel += eleDiff;
        cl.steps++;
        int oldLevel = elevatorLevel;
        int newLevel = cl.elevatorLevel;

        for (var index in permIndices) {
          cl.floors[newLevel].add(cl.floors[oldLevel][index]);
        }
        for (var index in permIndices.reversed) {
          cl.floors[oldLevel].removeAt(index);
        }

        if (cl.isValid()) futureStates.add(cl);
      }
    }
    return futureStates;
  }

  List<List<int>> getMovablePermIndices() {
    var permsToMove = <List<int>>[];
    var currentLevel = floors[elevatorLevel];

    for (int i = 0; i < currentLevel.length; i++) {
      for (int j = i + 1; j < currentLevel.length; j++) {
        permsToMove.add([i, j]);
      }
    }
    for (int i = 0; i < currentLevel.length; i++) {
      permsToMove.add([i]);
    }
    return permsToMove;
  }
}

State newInitialState(String input) {
  State s = State();
  var lines = LineSplitter.split(input).toList();

  for (int lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    var parts = lines[lineIndex].split(RegExp(r'\s+')).map((e) => e.replaceAll(RegExp(r'[,.]'), '')).toList();

    for (int i = 0; i < parts.length; i++) {
      if (parts[i] == "generator") {
        String material = parts[i - 1];
        s.floors[lineIndex].add(Halves(false, material));
      } else if (parts[i] == "microchip") {
        String material = parts[i - 1].split('-')[0];
        s.floors[lineIndex].add(Halves(true, material));
      }
    }
  }
  return s;
}
