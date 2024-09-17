from dataclasses import dataclass
from enum import IntEnum
from typing import Dict, List, Optional, Tuple
import sys

@dataclass(frozen=True)
class P:
    x: int
    y: int

class Dir(IntEnum):
    N = 0
    E = 1
    S = 2
    W = 3

    def rotate(self, direction: str) -> 'Dir':
        if direction == 'R':
            return Dir((self + 1) % 4)
        elif direction == 'L':
            return Dir((self - 1 + 4) % 4)
        return self

    def points(self) -> int:
        return (self + 3) % 4

@dataclass
class Movement:
    steps: int = 0
    rotate: Optional[str] = None

@dataclass
class Human:
    curr: P
    facing: Dir

    def walk(self, map_data: Dict[P, bool], dirs: List[P], size: int) -> Tuple[P, Dir]:
        dir_delta = dirs[self.facing]
        next_pos = P(self.curr.x + dir_delta.x, self.curr.y + dir_delta.y)
        if next_pos in map_data:
            if map_data[next_pos]:
                return self.curr, self.facing  # Hit a wall
            else:
                return next_pos, self.facing
        else:
            # Handle border crossing
            new_pos, new_facing = cross_border(next_pos, self.facing, size)
            if map_data.get(new_pos, False):
                return self.curr, self.facing  # Hit a wall after crossing
            return new_pos, new_facing

def cross_border(n: P, dir: Dir, size: int) -> Tuple[P, Dir]:
    x, y = n.x, n.y
    S = size  # For brevity

    if x == -1 and y < 2 * S:
        return P(y + 2 * S, x + 1), Dir.E
    elif x == -1 and y >= 2 * S:
        return P(x + 4 * S, y - 2 * S), Dir.N
    elif x == S and dir == Dir.S:
        return P(y - S, x + S - 1), Dir.W
    elif x == 2 * S - 1 and dir == Dir.N:
        return P(y + S, x - S + 1), Dir.E
    elif x == 3 * S and dir == Dir.S:
        return P(y + 2 * S, x - 2 * S - 1), Dir.W
    elif x == 4 * S:
        return P(x - 4 * S, y + 2 * S), Dir.S
    elif y == -1 and x < 3 * S:
        return P(3 * S - 1 - x, y + S + 1), Dir.E
    elif y == -1 and x >= 3 * S:
        return P(y + 1, x - 2 * S), Dir.S
    elif y == S - 1 and x < S:
        return P(3 * S - 1 - x, y - S + 1), Dir.E
    elif y == S - 1 and x >= S and dir == Dir.W:
        return P(y + S + 1, x - S), Dir.S
    elif y == S and dir == Dir.E:
        return P(y + 2 * S - 1, x - 2 * S), Dir.N
    elif y == 2 * S and x < 2 * S and dir == Dir.E:
        return P(y - S - 1, x + S), Dir.N
    elif y == 2 * S and x >= 2 * S:
        return P(3 * S - 1 - x, y + S - 1), Dir.W
    elif y == 3 * S:
        return P(3 * S - 1 - x, y - S - 1), Dir.W
    else:
        raise Exception("Not a border crossing")

def parse_path(path: str) -> List[Movement]:
    movements = []
    acc = 0
    for char in path:
        if char in ('R', 'L'):
            if acc != 0:
                movements.append(Movement(steps=acc))
                acc = 0
            movements.append(Movement(rotate=char))
        elif char.isdigit():
            acc = acc * 10 + int(char)
    if acc != 0:
        movements.append(Movement(steps=acc))
    return movements

def parse_input(filename: str) -> Tuple[Dict[P, bool], int, List[Movement]]:
    map_data: Dict[P, bool] = {}
    size = 0
    movements: List[Movement] = []

    with open(filename, 'r') as file:
        lines = file.readlines()

    # Parse map
    r = 0
    for line in lines:
        line = line.rstrip('\n')
        if line == "":
            break
        if r == 0:
            size = len(line) // 3
        for c, char in enumerate(line):
            if char == ' ':
                continue
            elif char == '#':
                map_data[P(r, c)] = True
            elif char == '.':
                map_data[P(r, c)] = False
        r += 1

    # Parse movements
    # Assuming the movement instructions are after the blank line
    movement_line = lines[r + 1].strip()
    movements = parse_path(movement_line)

    return map_data, size, movements

def main():
    map_data, size, movements = parse_input("input.txt")
    dirs = [
        P(-1, 0),  # N
        P(0, 1),   # E
        P(1, 0),   # S
        P(0, -1)   # W
    ]

    human = Human(curr=P(0, size), facing=Dir.E)

    for mov in movements:
        if mov.rotate:
            human.facing = human.facing.rotate(mov.rotate)
        for _ in range(mov.steps):
            new_pos, new_facing = human.walk(map_data, dirs, size)
            if new_pos == human.curr and new_facing == human.facing:
                break  # Cannot move
            human.curr = new_pos
            human.facing = new_facing

    final_value = 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points()
    print(final_value)

if __name__ == "__main__":
    main()
