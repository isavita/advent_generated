
import 'dart:collection';
import 'dart:io';
import 'dart:math';

class Item {
  final dynamic obj;
  final int priority;

  Item(this.obj, this.priority);
}

class PriorityQueue {
  final List<Item> _heap = [];

  int get length => _heap.length;

  void push(Item item) {
    _heap.add(item);
    _siftUp(_heap.length - 1);
  }

  Item pop() {
    final item = _heap.first;
    final last = _heap.removeLast();
    if (_heap.isNotEmpty) {
      _heap[0] = last;
      _siftDown(0);
    }
    return item;
  }

  void _siftUp(int index) {
    while (index > 0) {
      final parentIndex = (index - 1) ~/ 2;
      if (_heap[index].priority > _heap[parentIndex].priority) {
        _swap(index, parentIndex);
        index = parentIndex;
      } else {
        break;
      }
    }
  }

  void _siftDown(int index) {
    while (true) {
      final leftChildIndex = 2 * index + 1;
      final rightChildIndex = 2 * index + 2;
      int largestIndex = index;

      if (leftChildIndex < _heap.length &&
          _heap[leftChildIndex].priority > _heap[largestIndex].priority) {
        largestIndex = leftChildIndex;
      }

      if (rightChildIndex < _heap.length &&
          _heap[rightChildIndex].priority > _heap[largestIndex].priority) {
        largestIndex = rightChildIndex;
      }

      if (largestIndex != index) {
        _swap(index, largestIndex);
        index = largestIndex;
      } else {
        break;
      }
    }
  }

  void _swap(int i, int j) {
    final temp = _heap[i];
    _heap[i] = _heap[j];
    _heap[j] = temp;
  }
}

final neighbors4 = [
  Point(0, 1),
  Point(0, -1),
  Point(1, 0),
  Point(-1, 0),
];

(Map<Point<int>, String>, Map<String, bool>, Point<int>) parse(String input) {
  final grid = <Point<int>, String>{};
  final keys = <String, bool>{};
  late Point<int> start;
  final lines = input.split('\n');
  for (var y = 0; y < lines.length; y++) {
    final line = lines[y];
    for (var x = 0; x < line.length; x++) {
      final b = line[x];
      final p = Point(x, y);
      grid[p] = b;
      if (b.compareTo('a') >= 0 && b.compareTo('z') <= 0) {
        keys[b] = true;
      } else if (b == '@') {
        start = p;
      }
    }
  }
  return (grid, keys, start);
}

class Step {
  List<Point<int>> p;
  int workerId;
  Map<String, bool> keys;

  Step(this.p, this.workerId, this.keys);

  Step clone() {
    final gg = Step(
      List.from(p),
      workerId,
      Map.from(keys),
    );
    return gg;
  }

  @override
  String toString() {
    final ks = keys.keys.toList()..sort();
    return '${p}_$workerId\_${ks.join()}';
  }
}

int optimalPath(
    Map<Point<int>, String> grid, Map<String, bool> keys, List<Point<int>> start) {
  final pq = PriorityQueue();
  final dist = <String, int>{};

  final s = Step(start, 0, keys);
  for (var i = 0; i < start.length; i++) {
    final ss = s.clone();
    ss.workerId = i;
    pq.push(Item(ss, 0));
    dist[ss.toString()] = 0;
  }

  while (pq.length > 0) {
    final curr = pq.pop().obj as Step;
    final currdist = dist[curr.toString()]!;
    if (curr.keys.isEmpty) {
      return currdist;
    }
    final nextdist = 1 + currdist;
    for (final n in neighbors4) {
      final nextPoint = Point(curr.p[curr.workerId].x + n.x, curr.p[curr.workerId].y + n.y);
      final b = grid[nextPoint];
      if (b == null || b == '#') {
        continue;
      }
      if (b.compareTo('A') >= 0 && b.compareTo('Z') <= 0) {
        if (curr.keys.containsKey(b.toLowerCase())) {
          continue;
        }
      }
      final next = curr.clone();
      next.p[next.workerId] = nextPoint;
      bool foundNewKey = false;
      if (b.compareTo('a') >= 0 && b.compareTo('z') <= 0) {
        if (next.keys.containsKey(b)) {
          foundNewKey = true;
          next.keys.remove(b);
        }
      }
      for (var i = 0; i < next.p.length; i++) {
        if (curr.workerId != i && !foundNewKey) {
          continue;
        }
        final worker = next.clone();
        worker.workerId = i;
        if (!dist.containsKey(worker.toString()) ||
            nextdist < dist[worker.toString()]!) {
          dist[worker.toString()] = nextdist;
          pq.push(Item(worker, -nextdist));
        }
      }
    }
  }
  throw StateError('No viable path');
}

String readAll(String filepath) {
  final file = File(filepath);
  if (!file.existsSync()) {
    throw StateError('File not found: $filepath');
  }
  return file.readAsStringSync();
}

void main() {
  final input = readAll('input.txt');
  final (grid, keys, start) = parse(input);

  grid[start] = '#';
  for (final n in neighbors4) {
    grid[Point(start.x + n.x, start.y + n.y)] = '#';
  }
  final starts = <Point<int>>[];
  for (final n in [Point(-1, -1), Point(-1, 1), Point(1, -1), Point(1, 1)]) {
    grid[Point(start.x + n.x, start.y + n.y)] = '@';
    starts.add(Point(start.x + n.x, start.y + n.y));
  }
  print(optimalPath(grid, keys, starts));
}
