import re
from collections import deque
from typing import Dict, List, Optional, Tuple

class Room:
    def __init__(self, name: str):
        self.name = name
        self.connections: Dict[str, Optional['Room']] = {}

class Mode:
    EXPLORE = 0
    NAVIGATE = 1
    TEST = 2

opposite = {"north": "south", "south": "north", "west": "east", "east": "west"}

class EmulatorStatus:
    HALTED = 0
    OUTPUT = 1
    WAITING_FOR_INPUT = 2

class Emulator:
    def __init__(self, program: List[int], input_values: Optional[List[int]] = None):
        self.memory = program.copy()
        self.input = input_values.copy() if input_values else []
        self.ip = 0
        self.relative_base = 0

    def write_string(self, s: str) -> Tuple[int, Optional[Exception]]:
        for char in s:
            self.input.append(ord(char))
        return len(s), None

    def emulate(self) -> Tuple[Optional[int], int]:
        while True:
            if self.ip >= len(self.memory):
                self.memory.extend([0] * (self.ip - len(self.memory) + 1))
            instruction = self.memory[self.ip]
            opcode = instruction % 100

            def get_parameter(offset: int) -> int:
                mode = (instruction // (10 ** (offset + 1))) % 10
                param = self.memory[self.ip + offset]
                if mode == 0:
                    if param >= len(self.memory):
                        self.memory.extend([0] * (param - len(self.memory) + 1))
                    return self.memory[param]
                elif mode == 1:
                    return param
                elif mode == 2:
                    address = self.relative_base + param
                    if address >= len(self.memory):
                        self.memory.extend([0] * (address - len(self.memory) + 1))
                    return self.memory[address]
                else:
                    raise Exception(f"Unknown parameter mode: {mode}")

            def get_write_address(offset: int) -> int:
                mode = (instruction // (10 ** (offset + 1))) % 10
                param = self.memory[self.ip + offset]
                if mode == 0:
                    address = param
                elif mode == 2:
                    address = self.relative_base + param
                else:
                    raise Exception(f"Invalid mode for writing: {mode}")
                if address >= len(self.memory):
                    self.memory.extend([0] * (address - len(self.memory) + 1))
                return address

            if opcode == 1:
                a, b, c = get_parameter(1), get_parameter(2), get_write_address(3)
                self.memory[c] = a + b
                self.ip += 4
            elif opcode == 2:
                a, b, c = get_parameter(1), get_parameter(2), get_write_address(3)
                self.memory[c] = a * b
                self.ip += 4
            elif opcode == 3:
                if not self.input:
                    return None, EmulatorStatus.WAITING_FOR_INPUT
                a = get_write_address(1)
                self.memory[a] = self.input.pop(0)
                self.ip += 2
            elif opcode == 4:
                a = get_parameter(1)
                self.ip += 2
                return a, EmulatorStatus.OUTPUT
            elif opcode == 5:
                a, b = get_parameter(1), get_parameter(2)
                self.ip = b if a != 0 else self.ip + 3
            elif opcode == 6:
                a, b = get_parameter(1), get_parameter(2)
                self.ip = b if a == 0 else self.ip + 3
            elif opcode == 7:
                a, b, c = get_parameter(1), get_parameter(2), get_write_address(3)
                self.memory[c] = 1 if a < b else 0
                self.ip += 4
            elif opcode == 8:
                a, b, c = get_parameter(1), get_parameter(2), get_write_address(3)
                self.memory[c] = 1 if a == b else 0
                self.ip += 4
            elif opcode == 9:
                a = get_parameter(1)
                self.relative_base += a
                self.ip += 2
            elif opcode == 99:
                return None, EmulatorStatus.HALTED
            else:
                raise Exception(f"Unknown opcode: {opcode} at position {self.ip}")

def read_file(filename: str) -> str:
    with open(filename, 'r') as f:
        return f.read().strip()

def to_int64(s: str) -> int:
    return int(s)

def find_path(from_room: Room, to_room: Room) -> Optional[List[Room]]:
    queue = deque([(from_room, [from_room])])
    visited = set([from_room.name])

    while queue:
        current, path = queue.popleft()
        if current == to_room:
            return path
        for neighbor in current.connections.values():
            if neighbor and neighbor.name not in visited:
                visited.add(neighbor.name)
                queue.append((neighbor, path + [neighbor]))
    return None

def main():
    text = read_file("input.txt")
    program = [to_int64(value) for value in text.split(",")]
    emulator = Emulator(program)

    def send_command(format_str: str, *args):
        cmd = format_str % args
        emulator.write_string(cmd)

    room_name_regex = re.compile(r'^== (.+) ==$')
    list_item_regex = re.compile(r'^- (.+)$')
    taken_regex = re.compile(r'^You take the (.+)\.$')
    dropped_regex = re.compile(r'^You drop the (.+)\.$')
    result_regex = re.compile(r'"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$')

    world: Dict[str, Room] = {}
    inventory: Dict[str, bool] = {}
    mode = Mode.EXPLORE
    path: List[Room] = []
    checkpoint: Optional[Room] = None
    floor: Optional[Room] = None
    test_dir: str = ""
    available_items: List[str] = []
    item_mask: int = 0
    last: Optional[Room] = None
    last_items: List[str] = []
    last_dir: str = ""
    output_builder: List[int] = []
    current_room: Optional[Room] = None

    while True:
        char, status = emulator.emulate()

        if status == EmulatorStatus.HALTED:
            output = ''.join(map(chr, output_builder))
            for line in output.split("\n"):
                match = result_regex.search(line)
                if match:
                    print(match.group(1))
                    return

        elif status == EmulatorStatus.OUTPUT:
            if char is not None:
                output_builder.append(char)

        elif status == EmulatorStatus.WAITING_FOR_INPUT:
            output = ''.join(map(chr, output_builder))
            output_builder = []

            items: List[str] = []
            lines = output.split("\n")
            i = 0
            while i < len(lines):
                line = lines[i].strip()

                if not line or line == "Command?":
                    i += 1
                    continue

                match = room_name_regex.match(line)
                if match:
                    name = match.group(1)
                    i += 1
                    while i < len(lines) and lines[i].strip() != "":
                        i += 1
                    if name not in world:
                        current_room = Room(name)
                        world[name] = current_room
                    else:
                        current_room = world[name]
                    items = []
                    continue

                if line == "Doors here lead:":
                    i += 1
                    while i < len(lines) and lines[i].strip() != "":
                        door_line = lines[i].strip()
                        match = list_item_regex.match(door_line)
                        if match and current_room:
                            direction = match.group(1)
                            if direction not in current_room.connections:
                                current_room.connections[direction] = None
                        i += 1
                    continue

                if line == "Items here:":
                    i += 1
                    while i < len(lines) and lines[i].strip() != "":
                        item_line = lines[i].strip()
                        match = list_item_regex.match(item_line)
                        if match:
                            item = match.group(1)
                            items.append(item)
                        i += 1
                    continue

                match = taken_regex.match(line)
                if match:
                    taken = match.group(1)
                    inventory[taken] = True
                    if last:
                        current_room = last
                        items = [item for item in last_items if item != taken]
                    i += 1
                    continue

                match = dropped_regex.match(line)
                if match:
                    dropped = match.group(1)
                    inventory[dropped] = False
                    if last:
                        current_room = last
                        items = last_items + [dropped]
                    i += 1
                    continue

                if line.startswith('A loud, robotic voice says "Alert!'):
                    if mode == Mode.EXPLORE:
                        if path:
                            path.pop()
                        checkpoint, floor, test_dir = last, current_room, last_dir
                        if checkpoint and test_dir:
                            checkpoint.connections[test_dir] = floor
                    last, last_items, last_dir = None, [], ""
                    i += 1
                    continue

                i += 1

            if last is not None and last_dir and current_room is not None:
                if last.connections.get(last_dir) is None:
                    last.connections[last_dir] = current_room
                    current_room.connections[opposite[last_dir]] = last

            last, last_items, last_dir = current_room, items, ""

            if mode == Mode.EXPLORE:
                blacklist = ["photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet"]
                for item in items:
                    if item not in blacklist:
                        send_command("take %s\n", item)
                        break
                else:
                    target = None
                    for direction, room in current_room.connections.items():
                        if room is None:
                            path.append(current_room)
                            target = direction
                            break

                    if target:
                        last_dir = target
                        send_command("%s\n", target)
                        continue

                    if path:
                        last_room = path.pop()
                        back_dir = None
                        for direction, room in current_room.connections.items():
                            if room == last_room:
                                back_dir = direction
                                break
                        if back_dir:
                            last_dir = back_dir
                            send_command("%s\n", back_dir)
                            continue
                        else:
                            raise Exception(f'Cannot go from "{current_room.name}" to "{last_room.name}"')

                    if checkpoint and floor:
                        new_path = find_path(current_room, checkpoint)
                        if new_path:
                            path = new_path[1:]
                        mode = Mode.NAVIGATE
                        continue

            elif mode == Mode.NAVIGATE:
                if path:
                    next_room = path.pop(0)
                    direction = None
                    for dir, room in current_room.connections.items():
                        if room == next_room:
                            direction = dir
                            break
                    if direction:
                        last_dir = direction
                        send_command("%s\n", direction)
                        continue
                    else:
                        raise Exception(f'Cannot go from "{current_room.name}" to "{next_room.name}"')
                else:
                    available_items = [item for item, has in inventory.items() if has]
                    item_mask = 0
                    mode = Mode.TEST

            elif mode == Mode.TEST:
                for index, item in enumerate(available_items):
                    target_state = (item_mask & (1 << index)) != 0
                    if inventory[item] != target_state:
                        action = "take" if target_state else "drop"
                        send_command("%s %s\n", action, item)
                        break
                else:
                    item_mask += 1
                    if test_dir:
                        send_command("%s\n", test_dir)
                        continue
                    else:
                        raise Exception("Test direction (test_dir) is not set.")
                continue

if __name__ == "__main__":
    main()
