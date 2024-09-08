import heapq
from collections import deque
from typing import List, Tuple, Dict, Set

class Unit:
    def __init__(self, type: str, x: int, y: int, attack: int = 3, hp: int = 200):
        self.type = type
        self.x = x
        self.y = y
        self.attack = attack
        self.hp = hp

    def __lt__(self, other):
        return (self.y, self.x) < (other.y, other.x)

class Game:
    def __init__(self, grid: List[str], elf_attack: int = 3):
        self.grid = [list(row) for row in grid]
        self.units = []
        self.elf_attack = elf_attack
        self.rounds = 0
        self.cache = {}

        for y, row in enumerate(self.grid):
            for x, cell in enumerate(row):
                if cell in 'EG':
                    attack = self.elf_attack if cell == 'E' else 3
                    self.units.append(Unit(cell, x, y, attack))

    def play(self) -> int:
        while True:
            if self.round():
                break
        return self.rounds * sum(unit.hp for unit in self.units if unit.hp > 0)

    def round(self) -> bool:
        self.units.sort()
        for unit in self.units:
            if unit.hp <= 0:
                continue
            if self.move(unit):
                return True
            self.attack(unit)
        self.rounds += 1
        return False

    def move(self, unit: Unit) -> bool:
        targets = [u for u in self.units if u.type != unit.type and u.hp > 0]
        if not targets:
            return True

        in_range = set((x, y) for target in targets
                       for x, y in self.adjacent_squares(target.x, target.y)
                       if self.grid[y][x] == '.')

        if (unit.x, unit.y) not in in_range:
            move = self.find_move(unit, in_range)
            if move:
                unit.x, unit.y = move

        return False

    def attack(self, unit: Unit):
        targets = [u for u in self.units if u.type != unit.type and u.hp > 0 and
                   abs(u.x - unit.x) + abs(u.y - unit.y) == 1]
        if targets:
            target = min(targets, key=lambda u: (u.hp, u.y, u.x))
            target.hp -= unit.attack
            if target.hp <= 0:
                self.grid[target.y][target.x] = '.'

    def find_move(self, unit: Unit, goals: Set[Tuple[int, int]]) -> Tuple[int, int]:
        key = (unit.x, unit.y, tuple(sorted(goals)))
        if key in self.cache:
            return self.cache[key]

        queue = [(0, unit.x, unit.y, [])]
        visited = set()

        while queue:
            dist, x, y, path = heapq.heappop(queue)
            if (x, y) in visited:
                continue
            visited.add((x, y))

            if (x, y) in goals:
                move = path[0] if path else None
                self.cache[key] = move
                return move

            for nx, ny in self.adjacent_squares(x, y):
                if self.grid[ny][nx] == '.' and (nx, ny) not in visited:
                    new_path = path + [(nx, ny)] if path else [(nx, ny)]
                    heapq.heappush(queue, (dist + 1, nx, ny, new_path))

        self.cache[key] = None
        return None

    def adjacent_squares(self, x: int, y: int) -> List[Tuple[int, int]]:
        return [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]

def solve_part1(grid: List[str]) -> int:
    return Game(grid).play()

def solve_part2(grid: List[str]) -> int:
    elf_count = sum(row.count('E') for row in grid)
    attack = 4
    while True:
        game = Game(grid, attack)
        outcome = game.play()
        if sum(unit.type == 'E' and unit.hp > 0 for unit in game.units) == elf_count:
            return outcome
        attack += 1

# Example usage
grid = [
    "#######",
    "#.G...#",
    "#...EG#",
    "#.#.#G#",
    "#..G#E#",
    "#.....#",
    "#######"
]

print("Part 1:", solve_part1(grid))
print("Part 2:", solve_part2(grid))
