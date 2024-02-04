
import itertools
import os

class Halves:
    def __init__(self, is_chip, material):
        self.is_chip = is_chip
        self.material = material

    def __str__(self):
        t_type = " generator" if not self.is_chip else " microchip"
        return f"{self.material}{t_type}"

class State:
    def __init__(self, floors=None, elevator_level=0, steps=0):
        self.floors = floors if floors is not None else [[] for _ in range(4)]
        self.elevator_level = elevator_level
        self.steps = steps

    def __str__(self):
        result = f"Level {self.elevator_level} x Steps {self.steps}\n"
        for i, floor in enumerate(self.floors):
            result += f"  {i}: {', '.join(str(h) for h in floor)}\n"
        return result

    def hash_key(self):
        gen_to_index = {}
        chip_to_index = {}
        for fl_index, floor in enumerate(self.floors):
            for half in floor:
                if half.is_chip:
                    chip_to_index[half.material] = fl_index
                else:
                    gen_to_index[half.material] = fl_index

        gen_chip_pairs = sorted(((gen_to_index[mat], chip_to_index[mat]) for mat in gen_to_index), key=lambda x: (x[0], x[1]))
        return str(self.elevator_level) + str(gen_chip_pairs)

    def is_valid(self):
        for floor in self.floors:
            gens_seen = {half.material for half in floor if not half.is_chip}
            if not gens_seen:
                continue
            for half in floor:
                if half.is_chip and half.material not in gens_seen:
                    return False
        return True

    def is_done(self):
        return all(len(floor) == 0 for floor in self.floors[:-1])

    def get_movable_perm_indices(self):
        current_level = self.floors[self.elevator_level]
        indices = list(range(len(current_level)))
        return [list(perm) for r in range(1, 3) for perm in itertools.combinations(indices, r)]

    def clone(self):
        return State([floor[:] for floor in self.floors], self.elevator_level, self.steps)

    def get_next_states(self):
        future_states = []
        movable_perm_indices = self.get_movable_perm_indices()
        ele_diffs = [d for d in (-1, 1) if 0 <= self.elevator_level + d < len(self.floors)]

        for ele_diff in ele_diffs:
            for perm_indices in movable_perm_indices:
                cl = self.clone()
                cl.elevator_level += ele_diff
                cl.steps += 1
                old_level, new_level = self.elevator_level, cl.elevator_level

                for index in perm_indices:
                    cl.floors[new_level].append(cl.floors[old_level][index])
                for index in sorted(perm_indices, reverse=True):
                    cl.floors[old_level].pop(index)

                if cl.is_valid():
                    future_states.append(cl)
        return future_states

def read_file(file_path):
    with open(file_path, 'r') as file:
        return file.read().strip()

def rtg_hell_day(input_str, part):
    lines = input_str.split('\n')
    floors = [[] for _ in range(4)]
    for line_index, line in enumerate(lines):
        parts = line.replace(',', '').replace('.', '').split()
        for i, word in enumerate(parts):
            if word == "generator":
                material = parts[i-1]
                floors[line_index].append(Halves(False, material))
            elif word == "microchip":
                material = parts[i-1].split("-comp")[0]
                floors[line_index].append(Halves(True, material))

    initial_state = State(floors)
    if part == 2:
        initial_state.floors[0].extend([
            Halves(False, "elerium"), Halves(True, "elerium"),
            Halves(False, "dilithium"), Halves(True, "dilithium")
        ])

    queue = [initial_state]
    prev_states = set()
    while queue:
        front = queue.pop(0)
        if front.is_done():
            return front.steps
        hash_key = front.hash_key()
        if hash_key in prev_states:
            continue
        prev_states.add(hash_key)
        queue.extend(front.get_next_states())
    return -1

if __name__ == "__main__":
    input_str = read_file("input.txt")
    print(rtg_hell_day(input_str, 2))
