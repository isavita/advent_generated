import math
from typing import List, Dict, Tuple

class Cave:
    def __init__(self, input: List[str], elf_power: int):
        self.units = []
        self.map = {}
        self.parse_map(input, elf_power)

    def parse_map(self, input: List[str], elf_power: int):
        for y, row in enumerate(input):
            for x, col in enumerate(row):
                kind = RUNE_KINDS.get(col, KIND_WALL)
                tile = Tile(kind, x, y, self.map)
                if is_unit(kind):
                    self.units.append(Unit(tile, kind, elf_power))
                self.map.setdefault(y, {})[x] = tile

    def status(self) -> Tuple[int, bool]:
        elves = goblins = False
        hp = 0
        for u in self.units:
            if u.hitpoints <= 0:
                continue
            if u.kind == KIND_ELF:
                elves = True
            else:
                goblins = True
            hp += u.hitpoints
        return hp, elves and goblins

    def remove_the_dead(self):
        self.units = [unit for unit in self.units if unit.hitpoints > 0]

    def remove_unit(self, u: 'Unit'):
        u.tile.kind = KIND_SPACE
        u.tile.unit = None
        u.tile = None

    def tick(self, stop_on_elf_death: bool) -> Tuple[bool, bool]:
        self.remove_the_dead()
        self.units.sort(key=lambda u: (u.tile.y, u.tile.x))
        for unit in self.units:
            if unit.hitpoints <= 0:
                continue
            if not unit.targets(self):
                return False, False
            unit.move(self)
            if unit.attack(self) and stop_on_elf_death:
                return False, True
        return True, False

class Tile:
    def __init__(self, kind: int, x: int, y: int, map: Dict[int, Dict[int, 'Tile']]):
        self.kind = kind
        self.x = x
        self.y = y
        self.map = map
        self.unit = None

    def walkable_neighbors(self) -> List['Tile']:
        neighbors = []
        for offset in OFFSETS:
            n = self.map.get(self.y + offset[1], {}).get(self.x + offset[0])
            if n and n.kind == KIND_SPACE:
                neighbors.append(n)
        return neighbors

class Unit:
    def __init__(self, tile: Tile, kind: int, elf_power: int):
        self.kind = kind
        self.hitpoints = DEFAULT_HITPOINTS
        self.power = DEFAULT_POWER if kind != KIND_ELF else elf_power
        self.tile = tile
        tile.unit = self

    def targets(self, c: Cave) -> bool:
        return any(unit.kind != self.kind and unit.hitpoints > 0 for unit in c.units)

    def next_tile(self, c: Cave) -> Tuple[Tile, Tile]:
        targets = []
        closest_target_distance = math.inf
        distances, path = find_walkable_tiles(c.map, self.tile)
        enemies = self.enemies(c)
        for enemy in enemies:
            for target in enemy.tile.walkable_neighbors():
                if target in distances and distances[target] <= closest_target_distance:
                    if distances[target] < closest_target_distance:
                        closest_target_distance = distances[target]
                        targets = []
                    targets.append(target)
        targets.sort(key=lambda t: (t.y, t.x))
        if targets:
            target = targets[0]
            current = target
            while path[current] != self.tile:
                current = path[current]
            return current, target
        return None, None

    def enemies(self, c: Cave) -> List['Unit']:
        return sorted((unit for unit in c.units if unit.kind != self.kind and unit.hitpoints > 0), key=lambda u: (u.tile.y, u.tile.x))

    def enemy_neighbor(self, c: Cave) -> 'Unit':
        target = None
        for offset in OFFSETS:
            t = c.map.get(self.tile.y + offset[1], {}).get(self.tile.x + offset[0])
            if t and t.unit and t.unit.kind != self.kind and t.unit.hitpoints > 0:
                if not target or t.unit.hitpoints < target.hitpoints:
                    target = t.unit
        return target

    def move(self, c: Cave):
        if self.enemy_neighbor(c):
            return
        next, _ = self.next_tile(c)
        if next:
            next.unit = self
            next.kind = self.kind
            self.tile.kind = KIND_SPACE
            self.tile.unit = None
            self.tile = next

    def attack(self, c: Cave) -> bool:
        enemy = self.enemy_neighbor(c)
        if enemy:
            killed = enemy.damage(c, self.power)
            return killed and enemy.kind == KIND_ELF
        return False

    def damage(self, c: Cave, damage: int) -> bool:
        self.hitpoints -= damage
        if self.hitpoints <= 0:
            c.remove_unit(self)
            return True
        return False

def find_walkable_tiles(map: Dict[int, Dict[int, Tile]], start: Tile) -> Tuple[Dict[Tile, int], Dict[Tile, Tile]]:
    frontier = [start]
    distance = {start: 0}
    came_from = {start: None}
    while frontier:
        current = frontier.pop(0)
        for next in current.walkable_neighbors():
            if next not in distance:
                frontier.append(next)
                distance[next] = distance[current] + 1
                came_from[next] = current
    return distance, came_from

def is_unit(bit: int) -> bool:
    return (KIND_ELF | KIND_GOBLIN) & bit != 0

def combat(input: List[str]) -> int:
    cave = Cave(input, DEFAULT_POWER)
    i = 1
    while True:
        hp, combat = cave.status()
        if not combat:
            return (i - 1) * hp
        clean_round, _ = cave.tick(False)
        if not clean_round:
            i -= 1
        i += 1
    return -1

KIND_SPACE = 1 << 0
KIND_ELF = 1 << 1
KIND_GOBLIN = 1 << 2
KIND_WALL = 1 << 3

RUNE_KINDS = {
    '.': KIND_SPACE,
    'E': KIND_ELF,
    'G': KIND_GOBLIN,
    '#': KIND_WALL,
}

OFFSETS = [
    (0, -1),
    (-1, 0),
    (1, 0),
    (0, 1),
]

DEFAULT_HITPOINTS = 200
DEFAULT_POWER = 3

with open("input.txt") as f:
    lines = [line.strip() for line in f]
    print(combat(lines))