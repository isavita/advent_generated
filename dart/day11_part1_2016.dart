
import 'dart:io';
import 'dart:collection';

class Halves {
  bool isChip;
  String material;

  Halves(this.isChip, this.material);

  @override
  String toString() {
    String tType = isChip ? ' microchip' : ' generator';
    return '$material$tType';
  }
}

class State {
  List<List<Halves>> floors;
  int elevatorLevel;
  int steps;

  State(this.floors, this.elevatorLevel, this.steps);

  State.clone(State other)
      : floors = other.floors.map((floor) => List<Halves>.from(floor)).toList(),
        elevatorLevel = other.elevatorLevel,
        steps = other.steps;

  bool isDone() {
    int lenSum = 0;
    for (int i = 0; i < 3; i++) {
      lenSum += floors[i].length;
    }
    return lenSum == 0;
  }

  String hashKey() {
    Map<String, int> mapGenToIndex = {};
    Map<String, int> mapChipToIndex = {};
    for (int flIndex = 0; flIndex < floors.length; flIndex++) {
      for (Halves half in floors[flIndex]) {
        if (half.isChip) {
          mapChipToIndex[half.material] = flIndex;
        } else {
          mapGenToIndex[half.material] = flIndex;
        }
      }
    }

    List<List<int>> genChipPairs = [];
    for (String material in mapGenToIndex.keys) {
      genChipPairs.add([mapGenToIndex[material]!, mapChipToIndex[material]!]);
    }

    genChipPairs.sort((a, b) {
      if (a[0] != b[0]) {
        return a[0] - b[0];
      }
      return a[1] - b[1];
    });

    return '$elevatorLevel$genChipPairs';
  }

  bool isValid() {
    for (int i = 0; i < floors.length; i++) {
      Map<String, bool> gensSeen = {};
      for (Halves half in floors[i]) {
        if (!half.isChip) {
          gensSeen[half.material] = true;
        }
      }

      if (gensSeen.isEmpty) {
        continue;
      }

      for (Halves half in floors[i]) {
        if (half.isChip && !gensSeen.containsKey(half.material)) {
          return false;
        }
      }
    }

    return true;
  }

  List<List<int>> getMovablePermIndices() {
    List<List<int>> permsToMove = [];
    List<Halves> currentLevel = floors[elevatorLevel];

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

  List<State> getNextStates() {
    List<State> futureStates = [];
    List<List<int>> movablePermIndices = getMovablePermIndices();
    List<int> eleDiffs = [];

    if (elevatorLevel < floors.length - 1) {
      eleDiffs.add(1);
    }
    if (elevatorLevel > 0) {
      eleDiffs.add(-1);
    }

    for (int eleDiff in eleDiffs) {
      for (List<int> permIndices in movablePermIndices) {
        State cl = State.clone(this);
        cl.elevatorLevel += eleDiff;
        cl.steps++;
        int oldLevel = elevatorLevel;
        int newLevel = cl.elevatorLevel;

        for (int index in permIndices) {
          cl.floors[newLevel].add(cl.floors[oldLevel][index]);
        }

        for (int inx = permIndices.length - 1; inx >= 0; inx--) {
          cl.floors[oldLevel].removeAt(permIndices[inx]);
        }

        if (cl.isValid()) {
          futureStates.add(cl);
        }
      }
    }

    return futureStates;
  }
}

State newInitialState(String input) {
  List<List<Halves>> floors = [[], [], [], []];
  int lineIndex = 0;
  for (String line in input.split('\n')) {
    List<String> parts = line.split(' ');
    for (int i = 0; i < parts.length; i++) {
      parts[i] = parts[i].replaceAll(RegExp(r'[.,]'), '');
    }

    for (int i = 0; i < parts.length; i++) {
      String word = parts[i];
      if (word == 'generator') {
        String material = parts[i - 1];
        floors[lineIndex].add(Halves(false, material));
      } else if (word == 'microchip') {
        String material = parts[i - 1].substring(0, parts[i - 1].indexOf('-comp'));
        floors[lineIndex].add(Halves(true, material));
      }
    }
    lineIndex++;
  }
  return State(floors, 0, 0);
}

int rtgHellDay(String input) {
  State currentState = newInitialState(input);
  Queue<State> queue = Queue();
  queue.add(currentState);
  Set<String> prevStates = {};

  while (queue.isNotEmpty) {
    State front = queue.removeFirst();

    if (front.isDone()) {
      return front.steps;
    }

    String hash = front.hashKey();
    if (prevStates.contains(hash)) {
      continue;
    }
    prevStates.add(hash);

    List<State> nextStates = front.getNextStates();
    queue.addAll(nextStates);
  }

  return -1;
}

void main() {
  String input = File('input.txt').readAsStringSync().trimRight();
  int ans = rtgHellDay(input);
  print(ans);
}
