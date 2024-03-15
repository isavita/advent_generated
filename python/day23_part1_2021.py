from typing import List, Tuple, Dict
import heapq

def amphipod(input_str: str) -> int:
    start = parse_input(input_str)
    room_coord_to_want_char = {
        (2, 3): "A", (3, 3): "A",
        (2, 5): "B", (3, 5): "B",
        (2, 7): "C", (3, 7): "C",
        (2, 9): "D", (3, 9): "D",
    }

    min_heap = MinHeap()
    min_heap.push(start)
    seen_grids = set()

    while min_heap:
        front = min_heap.pop()

        key = str(front.grid)
        if key in seen_grids:
            continue
        seen_grids.add(key)

        if front.all_done(room_coord_to_want_char):
            return front.energy_used

        unsettled_coords = front.get_unsettled_coords(room_coord_to_want_char)
        for unsettled_coord in unsettled_coords:
            ur, uc = unsettled_coord
            next_moves = front.get_next_possible_moves(unsettled_coord, room_coord_to_want_char)
            for next_coord in next_moves:
                nr, nc = next_coord
                if front.grid[nr][nc] != ".":
                    raise ValueError(f"should only be moving to walkable spaces, got {front.grid[nr][nc]} at {nr},{nc}")

                cp = front.copy()
                cp.energy_used += calc_energy(cp.grid[ur][uc], unsettled_coord, next_coord)
                cp.path += f"{cp.grid[ur][uc]}{unsettled_coord}->{next_coord}({cp.energy_used}),"
                cp.grid[nr][nc], cp.grid[ur][uc] = cp.grid[ur][uc], cp.grid[nr][nc]

                min_heap.push(cp)

    raise ValueError("should return from loop")

class State:
    def __init__(self, grid: List[List[str]], energy_used: int, path: str):
        self.grid = grid
        self.energy_used = energy_used
        self.path = path

    def __lt__(self, other):
        return self.energy_used < other.energy_used

    def __str__(self):
        return "\n".join(["".join(row) for row in self.grid]) + f"\nenergy: {self.energy_used}, path: {self.path}"

    def copy(self):
        return State(
            [row[:] for row in self.grid],
            self.energy_used,
            self.path,
        )

    def all_done(self, room_coord_to_want_char: Dict[Tuple[int, int], str]) -> bool:
        for coord, want in room_coord_to_want_char.items():
            if self.grid[coord[0]][coord[1]] != want:
                return False
        return True

    def get_unsettled_coords(self, room_coord_to_want_char: Dict[Tuple[int, int], str]) -> List[Tuple[int, int]]:
        unsettled = []

        for col in range(1, len(self.grid[0])):
            if any(c in "ABCD" for c in self.grid[1][col]):
                unsettled.append((1, col))

        for col in [3, 5, 7, 9]:
            room_full_from_back = True
            for row in range(len(self.grid) - 2, 1, -1):
                coord = (row, col)
                want_char = room_coord_to_want_char.get(coord)
                got_char = self.grid[row][col]
                if got_char != ".":
                    if got_char != want_char:
                        room_full_from_back = False
                        unsettled.append(coord)
                    elif got_char == want_char and not room_full_from_back:
                        unsettled.append(coord)
        return unsettled

    def get_next_possible_moves(self, unsettled_coord: Tuple[int, int], room_coord_to_want_char: Dict[Tuple[int, int], str]) -> List[Tuple[int, int]]:
        unsettled_char = self.grid[unsettled_coord[0]][unsettled_coord[1]]

        if unsettled_char not in "ABCD":
            raise ValueError(f"unexpected character to get next moves for {unsettled_char}")

        possible = []

        started_in_hallway = unsettled_coord[0] == 1

        queue = [unsettled_coord]
        seen = set()
        while queue:
            front = queue.pop(0)

            if front in seen:
                continue
            seen.add(front)

            if front != unsettled_coord:
                if front not in {(1, 3), (1, 5), (1, 7), (1, 9)}:
                    want_char = room_coord_to_want_char.get(front)
                    if want_char is None:
                        if not started_in_hallway:
                            possible.append(front)
                    elif want_char == unsettled_char:
                        is_stuck_amphipod = False
                        room_has_deeper_open_spaces = False
                        for r in range(front[0] + 1, len(self.grid) - 1):
                            char = self.grid[r][front[1]]
                            if char == ".":
                                room_has_deeper_open_spaces = True
                            elif char != "." and char != unsettled_char:
                                is_stuck_amphipod = True
                                break

                        if not room_has_deeper_open_spaces and not is_stuck_amphipod:
                            possible.append(front)

            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                next_row, next_col = front[0] + dr, front[1] + dc
                if 0 <= next_row < len(self.grid) and 0 <= next_col < len(self.grid[0]) and self.grid[next_row][next_col] == ".":
                    queue.append((next_row, next_col))

        return possible

def parse_input(input_str: str) -> State:
    grid = [list(line) for line in input_str.splitlines()]
    return State(grid, 0, "")

def calc_energy(char: str, start: Tuple[int, int], end: Tuple[int, int]) -> int:
    dist = abs(end[1] - start[1])
    dist += start[0] - 1
    dist += end[0] - 1

    energy_per_type = {"A": 1, "B": 10, "C": 100, "D": 1000}
    if char not in energy_per_type:
        raise ValueError(f"{char} should not call calc_energy()")
    return energy_per_type[char] * dist

class MinHeap:
    def __init__(self):
        self.heap = []

    def push(self, item):
        heapq.heappush(self.heap, item)

    def pop(self):
        return heapq.heappop(self.heap)

    def __bool__(self):
        return bool(self.heap)

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        input_str = f.read().strip()
    print(amphipod(input_str))