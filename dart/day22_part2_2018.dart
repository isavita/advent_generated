
import 'dart:collection';
import 'dart:io';

class Coord {
  final int x, y;
  Coord(this.x, this.y);

  List<Coord> neighbors() {
    List<Coord> n = [
      Coord(x + 1, y),
      Coord(x, y + 1),
    ];

    if (x > 0) {
      n.add(Coord(x - 1, y));
    }
    if (y > 0) {
      n.add(Coord(x, y - 1));
    }

    return n;
  }
}

const geologicY = 16807;
const geologicX = 48271;
const caveModulo = 20183;

const typeRocky = 0;
const typeWet = 1;
const typeNarrow = 2;

const toolNone = 1;
const toolTorch = 2;
const toolGear = 4;

class MapData {
  late Coord target;
  late int depth;
  Map<int, Map<int, int>> geologicIndicesCache = {};
  Map<int, Map<int, int>> erosionLevelsCache = {};

  MapData(String input) {
    geologicIndicesCache = {};
    erosionLevelsCache = {};

    List<String> lines = input.split('\n');
    depth = int.parse(lines[0].split(': ')[1]);
    List<String> targetCoords = lines[1].split(': ')[1].split(',');
    target = Coord(int.parse(targetCoords[0]), int.parse(targetCoords[1]));
  }

  int geologicIndex(int x, int y) {
    if (geologicIndicesCache.containsKey(y) &&
        geologicIndicesCache[y]!.containsKey(x)) {
      return geologicIndicesCache[y]![x]!;
    }

    if (!geologicIndicesCache.containsKey(y)) {
      geologicIndicesCache[y] = {};
    }

    int index;
    if ((x == 0 && y == 0) || (x == target.x && y == target.y)) {
      index = 0;
    } else if (y == 0) {
      index = x * geologicY;
    } else if (x == 0) {
      index = y * geologicX;
    } else {
      index = erosionLevel(x - 1, y) * erosionLevel(x, y - 1);
    }

    geologicIndicesCache[y]![x] = index;
    return index;
  }

  int erosionLevel(int x, int y) {
    if (erosionLevelsCache.containsKey(y) &&
        erosionLevelsCache[y]!.containsKey(x)) {
      return erosionLevelsCache[y]![x]!;
    }

    if (!erosionLevelsCache.containsKey(y)) {
      erosionLevelsCache[y] = {};
    }

    int level = (geologicIndex(x, y) + depth) % caveModulo;
    erosionLevelsCache[y]![x] = level;
    return level;
  }

  int type(int x, int y) {
    return erosionLevel(x, y) % 3;
  }

  List<Item> neighbors(Coord pos, int equip) {
    List<Item> n = [];

    for (Coord c in pos.neighbors()) {
      int t = type(c.x, c.y);

      if ((equip & allowed(t)) != 0) {
        n.add(Item(c, equip, 1));
        n.add(Item(c, equip ^ allowed(t), 8));
      }
    }

    return n;
  }

  int allowed(int regionType) {
    switch (regionType) {
      case typeRocky:
        return toolGear | toolTorch;
      case typeWet:
        return toolGear | toolNone;
      case typeNarrow:
        return toolTorch | toolNone;
      default:
        throw Exception("unknown region type: $regionType");
    }
  }
}

class Item {
  Coord pos;
  int equip;
  int time;

  Item(this.pos, this.equip, this.time);
}

class PriorityQueue {
  List<Item> _heap = [];

  void add(Item item) {
    _heap.add(item);
    _heapifyUp(_heap.length - 1);
  }

  Item remove() {
    Item result = _heap[0];
    _heap[0] = _heap[_heap.length - 1];
    _heap.removeLast();
    _heapifyDown(0);
    return result;
  }

  bool get isEmpty => _heap.isEmpty;

  void _heapifyUp(int index) {
    while (index > 0) {
      int parentIndex = (index - 1) ~/ 2;
      if (_heap[index].time < _heap[parentIndex].time) {
        _swap(index, parentIndex);
        index = parentIndex;
      } else {
        break;
      }
    }
  }

  void _heapifyDown(int index) {
    while (true) {
      int leftChild = 2 * index + 1;
      int rightChild = 2 * index + 2;
      int smallest = index;

      if (leftChild < _heap.length && _heap[leftChild].time < _heap[smallest].time) {
        smallest = leftChild;
      }

      if (rightChild < _heap.length && _heap[rightChild].time < _heap[smallest].time) {
        smallest = rightChild;
      }

      if (smallest != index) {
        _swap(index, smallest);
        index = smallest;
      } else {
        break;
      }
    }
  }

  void _swap(int i, int j) {
    Item temp = _heap[i];
    _heap[i] = _heap[j];
    _heap[j] = temp;
  }
}

const bailFactor = 8;

int rescue(String input) {
  MapData m = MapData(input);

  PriorityQueue queue = PriorityQueue();
  queue.add(Item(Coord(0, 0), toolTorch, 0));

  Map<String, int> distances = {
    "${Coord(0, 0).x},${Coord(0, 0).y},$toolTorch": 0,
  };

  while (!queue.isEmpty) {
    Item item = queue.remove();

    if (item.pos.x == m.target.x &&
        item.pos.y == m.target.y &&
        item.equip == toolTorch) {
      return item.time;
    }

    if (item.pos.x > bailFactor * m.target.x ||
        item.pos.y > bailFactor * m.target.y) {
      continue;
    }

    String key = "${item.pos.x},${item.pos.y},${item.equip}";
    if (distances.containsKey(key) && distances[key]! < item.time) {
      continue;
    }

    for (Item n in m.neighbors(item.pos, item.equip)) {
      String dKey = "${n.pos.x},${n.pos.y},${n.equip}";

      if (!distances.containsKey(dKey) ||
          item.time + n.time < distances[dKey]!) {
        distances[dKey] = item.time + n.time;
        queue.add(Item(n.pos, n.equip, item.time + n.time));
      }
    }
  }

  return 0;
}

void main() {
  String input = File("input.txt").readAsStringSync();
  print(rescue(input));
}

