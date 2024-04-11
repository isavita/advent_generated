import heapq
from collections import defaultdict

class Machine:
    def __init__(self, program, input_queue, output_queue):
        self.data = defaultdict(int, enumerate(program))
        self.ip = 0
        self.input_queue = input_queue
        self.output_queue = output_queue
        self.relbase = 0

    def get(self, i, mo):
        if mo == 0:
            return self.data[self.data[i]]
        elif mo == 1:
            return self.data[i]
        elif mo == 2:
            return self.data[self.relbase + self.data[i]]
        else:
            raise ValueError(f"Unknown mode: {mo}")

    def set(self, i, mo, val):
        if mo == 0:
            self.data[self.data[i]] = val
        elif mo == 2:
            self.data[self.relbase + self.data[i]] = val
        else:
            raise ValueError(f"Unknown mode: {mo}")

    def step(self):
        op = self.data[self.ip] % 100
        modes = [self.data[self.ip] // 10**i % 10 for i in range(2, 5)]

        if op == 1:
            self.set(self.ip+3, modes[2], self.get(self.ip+1, modes[0]) + self.get(self.ip+2, modes[1]))
            self.ip += 4
        elif op == 2:
            self.set(self.ip+3, modes[2], self.get(self.ip+1, modes[0]) * self.get(self.ip+2, modes[1]))
            self.ip += 4
        elif op == 3:
            if not self.input_queue:
                return False
            self.set(self.ip+1, modes[0], self.input_queue.pop(0))
            self.ip += 2
        elif op == 4:
            self.output_queue.append(self.get(self.ip+1, modes[0]))
            self.ip += 2
        elif op == 5:
            self.ip = self.get(self.ip+2, modes[1]) if self.get(self.ip+1, modes[0]) != 0 else self.ip+3
        elif op == 6:
            self.ip = self.get(self.ip+2, modes[1]) if self.get(self.ip+1, modes[0]) == 0 else self.ip+3
        elif op == 7:
            self.set(self.ip+3, modes[2], 1 if self.get(self.ip+1, modes[0]) < self.get(self.ip+2, modes[1]) else 0)
            self.ip += 4
        elif op == 8:
            self.set(self.ip+3, modes[2], 1 if self.get(self.ip+1, modes[0]) == self.get(self.ip+2, modes[1]) else 0)
            self.ip += 4
        elif op == 9:
            self.relbase += self.get(self.ip+1, modes[0])
            self.ip += 2
        elif op == 99:
            return False
        else:
            raise ValueError(f"Unknown opcode: {op}")

        return True

    def run(self):
        while self.step():
            pass

def manhattan(p, q):
    return abs(p[0]-q[0]) + abs(p[1]-q[1])

class Pathfinder:
    def __init__(self, program):
        self.machine = Machine(program, [], [])
        self.grid = {(0, 0): '.'}
        self.dirmap = {1: (0, 1), 2: (0, -1), 3: (-1, 0), 4: (1, 0)}
        self.p = (0, 0)
        self.oxygen = None

    def try_move(self, dir):
        self.machine.input_queue.append(dir)
        self.machine.run()
        if not self.machine.output_queue:
            return False
        output = self.machine.output_queue.pop()
        next_pos = (self.p[0] + self.dirmap[dir][0], self.p[1] + self.dirmap[dir][1])

        if output == 0:
            self.grid[next_pos] = '#'
            return False
        elif output == 1:
            self.grid[next_pos] = '.'
        elif output == 2:
            self.grid[next_pos] = 'O'
            self.oxygen = next_pos

        self.p = next_pos
        return True

    def explore(self):
        while self.open():
            if self.p not in self.open():
                min_dist = float('inf')
                next_pos = None
                for pos in self.open():
                    dist = manhattan(self.p, pos)
                    if dist < min_dist:
                        min_dist = dist
                        next_pos = pos

                for move in self.shortest_path(self.p, next_pos):
                    if not self.try_move(move):
                        raise ValueError("Bad path")

            while True:
                for dir in self.dirmap:
                    next_pos = (self.p[0] + self.dirmap[dir][0], self.p[1] + self.dirmap[dir][1])
                    if next_pos not in self.grid:
                        break
                else:
                    break

                if not self.try_move(dir):
                    break

    def open(self):
        return {pos for pos, val in self.grid.items() if val != '#' and any(
            (pos[0]+n[0], pos[1]+n[1]) not in self.grid for n in self.dirmap.values())}

    def shortest_path(self, start, end):
        queue = [(0, start, [])]
        visited = set()

        while queue:
            dist, pos, path = heapq.heappop(queue)
            if pos == end:
                return path
            if pos in visited:
                continue
            visited.add(pos)

            for dir, delta in self.dirmap.items():
                next_pos = (pos[0] + delta[0], pos[1] + delta[1])
                if next_pos in self.grid and self.grid[next_pos] != '#':
                    heapq.heappush(queue, (dist+1, next_pos, path+[dir]))

        raise ValueError("No path found")

    def longest_path(self, start):
        queue = [(0, start)]
        distances = {start: 0}

        while queue:
            dist, pos = heapq.heappop(queue)
            for delta in self.dirmap.values():
                next_pos = (pos[0] + delta[0], pos[1] + delta[1])
                if next_pos in self.grid and self.grid[next_pos] != '#' and (
                        next_pos not in distances or distances[next_pos] > dist+1):
                    distances[next_pos] = dist+1
                    heapq.heappush(queue, (dist+1, next_pos))

        return max(distances.values())

def main():
    with open("input.txt") as f:
        program = list(map(int, f.read().strip().split(',')))

    pathfinder = Pathfinder(program)
    pathfinder.explore()
    print(pathfinder.longest_path(pathfinder.oxygen))

if __name__ == "__main__":
    main()