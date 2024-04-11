class Elf:
    def __init__(self, x, y):
        self.pos = (x, y)
        self.moving = False
        self.next_pos = None

    def around_all_empty(self, map_, dirs):
        for dx, dy in dirs:
            adj = (self.pos[0] + dx, self.pos[1] + dy)
            if adj in map_:
                return False
        return True

    def elf_in_direction(self, wanna_go, map_, dirs):
        for j in (-1, 0, 1):
            dxy = dirs[(wanna_go + j + 8) % 8]
            adj = (self.pos[0] + dxy[0], self.pos[1] + dxy[1])
            if adj in map_:
                return True
        return False

def parse():
    elves = []
    map_ = {}
    with open('input.txt', 'r') as file:
        for row, line in enumerate(file):
            for col, char in enumerate(line.strip()):
                if char == '#':
                    pos = (row, col)
                    map_[pos] = None
                    elves.append(Elf(row, col))
    return elves, map_

def run(elves, map_, order, curr_dir, dirs):
    proposes = {}
    for e in elves:
        if e.around_all_empty(map_, dirs):
            continue
        for i in range(4):
            dir_ = order[(curr_dir + i) % 4]
            if e.elf_in_direction(dir_, map_, dirs):
                continue
            dxy = dirs[dir_]
            dest = (e.pos[0] + dxy[0], e.pos[1] + dxy[1])
            if dest in proposes:
                proposes[dest] += 1
            else:
                proposes[dest] = 1
            e.next_pos = dest
            e.moving = True
            break

    someone_moved = False
    for e in elves:
        if not e.moving:
            continue
        if proposes[e.next_pos] > 1:
            e.moving = False
            continue
        someone_moved = True
        del map_[e.pos]
        map_[e.next_pos] = None
        e.pos = e.next_pos
        e.moving = False

    return someone_moved

def main():
    dirs = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]
    order = [1, 5, 7, 3]  # N, S, W, E (mapping to dirs indices)
    curr_dir = 0
    
    elves, map_ = parse()
    
    i = 0
    while True:
        if not run(elves, map_, order, curr_dir, dirs):
            print(i + 1)
            break
        curr_dir = (curr_dir + 1) % 4
        i += 1

if __name__ == '__main__':
    main()