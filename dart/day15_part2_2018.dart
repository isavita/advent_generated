
import 'dart:io';
import 'dart:collection';
import 'dart:math';

const int kindSpace = 1 << 0;
const int kindElf = 1 << 1;
const int kindGoblin = 1 << 2;
const int kindWall = 1 << 3;

final Map<int, String> kindRunes = {
  kindSpace: '.',
  kindElf: 'E',
  kindGoblin: 'G',
  kindWall: '#',
};

final Map<String, int> runeKinds = {
  '.': kindSpace,
  'E': kindElf,
  'G': kindGoblin,
  '#': kindWall,
};

bool isUnit(int bit) {
  return (kindElf | kindGoblin) & bit != 0;
}

class Coordinate {
  final int x, y;
  Coordinate(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Coordinate &&
          runtimeType == other.runtimeType &&
          x == other.x &&
          y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

final List<Coordinate> offsets = [
  Coordinate(0, -1), // Up
  Coordinate(-1, 0), // Left
  Coordinate(1, 0), // Right
  Coordinate(0, 1), // Down
];

class Tile implements Comparable<Tile> {
  int kind;
  final int x, y;
  GameMap map;
  Unit? unit;

  Tile(this.kind, this.x, this.y, this.map, {this.unit});

  List<Tile> walkableNeighbors() {
    List<Tile> neighbors = [];
    for (var offset in offsets) {
      Tile? n = map.tile(x + offset.x, y + offset.y);
      if (n != null && n.kind == kindSpace) {
        neighbors.add(n);
      }
    }
    return neighbors;
  }

  @override
  int compareTo(Tile other) {
    if (y == other.y) {
      return x.compareTo(other.x);
    }
    return y.compareTo(other.y);
  }
    @override
  String toString() => 'Tile($x, $y, ${kindRunes[kind]})';
}

class Unit implements Comparable<Unit> {
  final int kind;
  int hitpoints;
  int power;
  Tile tile;

  static const int defaultHitpoints = 200;
  static const int defaultPower = 3;

  Unit(this.kind, this.tile, int elfPower)
      : hitpoints = defaultHitpoints,
        power = (kind == kindElf) ? elfPower : defaultPower {
    tile.unit = this;
  }

  bool targets(Cave c) {
    return c.units.any((u) => u.kind != kind && u.hitpoints > 0);
  }

  Tile? nextTile(Cave c) {
    List<Tile> targets = [];
    int closestTargetDistance = 999999; // Simulate MaxInt

    var bfsResult = c.map.findWalkableTiles(tile);
    Map<Tile, int> distances = bfsResult.item1;
    Map<Tile, Tile> path = bfsResult.item2;

    List<Unit> enemies = this.enemies(c);

    for (var enemy in enemies) {
      for (var target in enemy.tile.walkableNeighbors()) {
        if (distances.containsKey(target)) {
          int distance = distances[target]!;
          if (distance <= closestTargetDistance) {
            if (distance < closestTargetDistance) {
              closestTargetDistance = distance;
              targets = [];
            }
            targets.add(target);
          }
        }
      }
    }

    if (targets.isEmpty) {
      return null;
    }

    targets.sort(); // Sort by reading order (using Tile's compareTo)
    Tile bestTarget = targets[0];
    Tile? current = bestTarget;

    // Backtrack to find the first step from the current unit's tile
    while (current != null && path[current] != tile) {
        current = path[current];
    }
    return current; // This is the first step tile
  }

  List<Unit> enemies(Cave c) {
    List<Unit> enemies = c.units
        .where((unit) => unit.kind != kind && unit.hitpoints > 0)
        .toList();
    enemies.sort(); // Sort by reading order (using Unit's compareTo)
    return enemies;
  }

  Unit? enemyNeighbor(Cave c) {
    Unit? target;
    for (var offset in offsets) {
      Tile? t = c.map.tile(tile.x + offset.x, tile.y + offset.y);
      if (t?.unit != null && t!.unit!.kind != kind && t.unit!.hitpoints > 0) {
        if (target == null || t.unit!.hitpoints < target.hitpoints) {
          target = t.unit;
        } else if (t.unit!.hitpoints == target.hitpoints) {
           // Tie-breaking: choose the one first in reading order
           if(t.compareTo(target.tile) < 0) {
               target = t.unit;
           }
        }
      }
    }
    return target;
  }

  void move(Cave c) {
    if (enemyNeighbor(c) != null) {
      return; // Already in range
    }
    Tile? next = nextTile(c);
    if (next != null) {
      tile.unit = null;
      tile.kind = kindSpace;

      next.unit = this;
      next.kind = kind;
      tile = next;
    }
  }

  // Returns true if an elf died as a result of this attack
  bool attack(Cave c) {
    Unit? enemy = enemyNeighbor(c);
    if (enemy != null) {
      bool killed = enemy.damage(c, power);
      return killed && enemy.kind == kindElf;
    }
    return false;
  }

  // Returns true if the unit died
  bool damage(Cave c, int damage) {
    hitpoints -= damage;
    if (hitpoints <= 0) {
      c.removeUnit(this);
      return true;
    }
    return false;
  }

  @override
  int compareTo(Unit other) {
    return tile.compareTo(other.tile);
  }
}

class GameMap {
  Map<int, Map<int, Tile>> _grid = {};
   int maxY = 0;
   int maxX = 0;

  void setTile(Tile t, int x, int y) {
     if (!_grid.containsKey(y)) {
        _grid[y] = {};
     }
     _grid[y]![x] = t;
     maxY = max(maxY, y);
     maxX = max(maxX, _grid[y]!.keys.reduce(max));
  }


  Tile? tile(int x, int y) {
    return _grid[y]?[x];
  }

  // Returns distances and paths (cameFrom)
  Tuple<Map<Tile, int>, Map<Tile, Tile>> findWalkableTiles(Tile start) {
    Queue<Tile> frontier = Queue<Tile>();
    frontier.add(start);
    Map<Tile, int> distance = {start: 0};
    Map<Tile, Tile> cameFrom = {start: start}; // Use start to indicate no predecessor

    while (frontier.isNotEmpty) {
      Tile current = frontier.removeFirst();

      for (var next in current.walkableNeighbors()..sort()) { // Sort neighbors ensures deterministic path selection for tie-breaking
        if (!distance.containsKey(next)) {
          frontier.add(next);
          distance[next] = distance[current]! + 1;
          cameFrom[next] = current;
        }
      }
    }
    cameFrom.remove(start); // Remove the self-reference for the start node
    return Tuple(distance, cameFrom);
  }
}

// Simple Tuple class
class Tuple<T1, T2> {
  final T1 item1;
  final T2 item2;
  Tuple(this.item1, this.item2);
}


class Cave {
  List<Unit> units = [];
  GameMap map = GameMap();

  Cave(List<String> input, int elfPower) {
    parseMap(input, elfPower);
  }

  void parseMap(List<String> input, int elfPower) {
    map = GameMap();
    units = [];
    for (int y = 0; y < input.length; y++) {
      String row = input[y];
      for (int x = 0; x < row.length; x++) {
        String char = row[x];
        int kind = runeKinds[char] ?? kindWall;
        Tile tile = Tile(kind, x, y, map);
        if (isUnit(kind)) {
          units.add(Unit(kind, tile, elfPower));
        }
        map.setTile(tile, x, y);
      }
    }
  }

   // Returns total HP and combat status (true if ongoing)
   Tuple<int, bool> status() {
    bool elves = false;
    bool goblins = false;
    int hp = 0;

    for (var u in units) {
      if (u.hitpoints <= 0) continue;
      if (u.kind == kindElf) {
        elves = true;
      } else {
        goblins = true;
      }
      hp += u.hitpoints;
    }

    return Tuple(hp, elves && goblins);
  }

  void removeTheDead() {
    units.removeWhere((unit) => unit.hitpoints <= 0);
  }

  void removeUnit(Unit u) {
     u.tile.kind = kindSpace;
     u.tile.unit = null;
     // The unit object itself will be removed from the list by removeTheDead
  }

  // Returns Tuple(combatContinues, elfDied)
  Tuple<bool, bool> tick(bool stopOnElfDeath) {
    removeTheDead();
    units.sort(); // Sort by reading order

    for (int i = 0; i < units.length; i++) {
      Unit unit = units[i];
      if (unit.hitpoints <= 0) {
         continue; // Unit might have died earlier in this same round
      }

      if (!unit.targets(this)) {
        return Tuple(false, false); // Combat ends immediately
      }

      unit.move(this);
      bool elfDied = unit.attack(this);
      if (elfDied && stopOnElfDeath) {
        return Tuple(false, true); // Combat ends due to elf death
      }
    }
    return Tuple(true, false); // Round completed without ending conditions met
  }

}


int cheatingElves(List<String> input) {
  for (int power = 4; ; power++) {
    Cave cave = Cave(input, power);
    bool elfDied = false;
    int rounds = 0;

    while(true) {
        var currentStatus = cave.status();
        if (!currentStatus.item2) { // Combat ended?
             if (!elfDied) {
                 return rounds * currentStatus.item1;
             } else {
                 break; // Elf died, try higher power
             }
        }

        var tickResult = cave.tick(true); // stopOnElfDeath = true
        bool combatContinues = tickResult.item1;
        elfDied = tickResult.item2;

        if (elfDied) {
            break; // Elf died, try higher power
        }

        if (combatContinues) { // Only increment round if combat didn't end during the tick
            rounds++;
        } else {
             // Re-check status after the tick that ended combat
             currentStatus = cave.status();
             if (!elfDied) {
                 return rounds * currentStatus.item1;
             } else {
                 // This case shouldn't happen if elfDied was checked correctly above,
                 // but break defensively.
                 break;
             }
        }
    }
  }
}


void main() {
  try {
    final file = File('input.txt');
    final lines = file.readAsLinesSync().where((l) => l.trim().isNotEmpty).toList();
    final result = cheatingElves(lines);
    print(result);
  } catch (e) {
    print('Error reading file or processing input: $e');
  }
}
