
import 'dart:io';
import 'dart:math';

class Coordinate {
  final int x, y;
  Coordinate(this.x, this.y);
}

final offsets = [
  Coordinate(0, -1),
  Coordinate(-1, 0),
  Coordinate(1, 0),
  Coordinate(0, 1),
];

class Tile {
  int kind;
  int x, y;
  Map<int, Map<int, Tile>> map;
  Unit? unit;

  Tile(this.kind, this.x, this.y, this.map);

  List<Tile> walkableNeighbors() {
    var neighbors = <Tile>[];
    for (var offset in offsets) {
      var nx = x + offset.x;
      var ny = y + offset.y;
      if (map.containsKey(ny) && map[ny]!.containsKey(nx)) {
        var n = map[ny]![nx]!;
        if (n.kind == KindSpace) {
          neighbors.add(n);
        }
      }
    }
    return neighbors;
  }
}

class Unit {
  int kind;
  int hitpoints;
  int power;
  Tile tile;

  Unit(this.kind, this.hitpoints, this.power, this.tile);

  List<Unit> enemies(Cave cave) {
    return cave.units.where((u) => u.kind != kind && u.hitpoints > 0).toList()
      ..sort((a, b) {
        if (a.tile.y == b.tile.y) {
          return a.tile.x.compareTo(b.tile.x);
        }
        return a.tile.y.compareTo(b.tile.y);
      });
  }

  Unit? enemyNeighbor(Cave cave) {
    Unit? target;
    for (var offset in offsets) {
      var nx = tile.x + offset.x;
      var ny = tile.y + offset.y;
      if (cave.map.containsKey(ny) && cave.map[ny]!.containsKey(nx)) {
        var t = cave.map[ny]![nx]!;
        if (t.unit != null && t.unit!.kind != kind && t.unit!.hitpoints > 0) {
          if (target == null || t.unit!.hitpoints < target.hitpoints) {
            target = t.unit;
          }
        }
      }
    }
    return target;
  }

  void move(Cave cave) {
    if (enemyNeighbor(cave) != null) {
      return;
    }
    var next = nextTile(cave);
    if (next != null) {
      next.unit = this;
      next.kind = kind;
      tile.kind = KindSpace;
      tile.unit = null;
      tile = next;
    }
  }

  bool attack(Cave cave) {
    var enemy = enemyNeighbor(cave);
    if (enemy != null) {
      return enemy.damage(cave, power);
    }
    return false;
  }

  bool damage(Cave cave, int damage) {
    hitpoints -= damage;
    if (hitpoints <= 0) {
      cave.removeUnit(this);
      return true;
    }
    return false;
  }

  Tile? nextTile(Cave cave) {
    var targets = <Tile>[];
    var closestTargetDistance = (1 << 31) - 1;
    var distances = <Tile, int>{};
    var cameFrom = <Tile, Tile?>{};
    var frontier = <Tile>[tile];
    distances[tile] = 0;
    cameFrom[tile] = null;

    while (frontier.isNotEmpty) {
      var current = frontier.removeAt(0);
      for (var next in current.walkableNeighbors()) {
        if (!distances.containsKey(next)) {
          frontier.add(next);
          distances[next] = distances[current]! + 1;
          cameFrom[next] = current;
        }
      }
    }

    var enemies = this.enemies(cave);
    for (var enemy in enemies) {
      for (var target in enemy.tile.walkableNeighbors()) {
        if (distances.containsKey(target)) {
          var distance = distances[target]!;
          if (distance <= closestTargetDistance) {
            if (distance < closestTargetDistance) {
              closestTargetDistance = distance;
              targets.clear();
            }
            targets.add(target);
          }
        }
      }
    }
    targets.sort((a, b) {
      if (a.y == b.y) {
        return a.x.compareTo(b.x);
      }
      return a.y.compareTo(b.y);
    });
    if (targets.isNotEmpty) {
      var target = targets[0];
      var current = target;
      while (true) {
        if (cameFrom[current] == tile) {
          return current;
        }
        current = cameFrom[current]!;
      }
    }
    return null;
  }
}

class Cave {
  List<Unit> units = [];
  Map<int, Map<int, Tile>> map = {};

  Cave(List<String> input, int elfPower) {
    parseMap(input, elfPower);
  }

  void parseMap(List<String> input, int elfPower) {
    for (var y = 0; y < input.length; y++) {
      for (var x = 0; x < input[y].length; x++) {
        var char = input[y][x];
        var kind = RuneKinds[char]!;
        var tile = Tile(kind, x, y, map);
        if (map[y] == null) {
          map[y] = {};
        }
        map[y]![x] = tile;
        if (kind == KindElf || kind == KindGoblin) {
          units.add(Unit(kind, defaultHitpoints,
              kind == KindElf ? elfPower : defaultPower, tile));
          tile.unit = units.last;
        }
      }
    }
  }

  int status() {
    var elves = false;
    var goblins = false;
    var hp = 0;

    for (var u in units) {
      if (u.hitpoints <= 0) continue;
      if (u.kind == KindElf) {
        elves = true;
      } else {
        goblins = true;
      }
      hp += u.hitpoints;
    }
    return elves && goblins ? -1 : hp;
  }

  void removeTheDead() {
    units.removeWhere((u) => u.hitpoints <= 0);
  }

  void removeUnit(Unit u) {
    u.tile.kind = KindSpace;
    u.tile.unit = null;
  }

  bool tick() {
    removeTheDead();
    units.sort((a, b) {
      if (a.tile.y == b.tile.y) {
        return a.tile.x.compareTo(b.tile.x);
      }
      return a.tile.y.compareTo(b.tile.y);
    });
    for (var unit in units.toList()) {
      if (unit.hitpoints <= 0) continue;
      if (!unit.enemies(this).isNotEmpty) return false;
      unit.move(this);
      unit.attack(this);
    }
    return true;
  }
}

const KindSpace = 1;
const KindElf = 2;
const KindGoblin = 4;
const KindWall = 8;
const defaultHitpoints = 200;
const defaultPower = 3;

final RuneKinds = {
  '.': KindSpace,
  'E': KindElf,
  'G': KindGoblin,
  '#': KindWall,
};

int combat(List<String> input) {
  var cave = Cave(input, defaultPower);
  var i = 0;
  while (true) {
    i++;
    var result = cave.status();
    if (result != -1) {
      return (i - 1) * result;
    }
    if (!cave.tick()) {
      i--;
    }
  }
}

void main() {
  var input = File('input.txt').readAsLinesSync();
  print(combat(input));
}
