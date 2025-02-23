
import * as fs from 'fs';

class Room {
  name: string;
  connections: { [key: string]: Room | null } = {};
  constructor(name: string) {
    this.name = name;
  }
}

const opposite: { [key: string]: string } = {
  north: 'south',
  south: 'north',
  west: 'east',
  east: 'west',
};

enum EmulatorStatus {
  HALTED,
  OUTPUT,
  WAITING_FOR_INPUT,
}

class Emulator {
  memory: number[];
  input: number[];
  ip: number;
  relativeBase: number;

  constructor(program: number[], inputValues: number[] = []) {
    this.memory = [...program];
    this.input = [...inputValues];
    this.ip = 0;
    this.relativeBase = 0;
  }

  writeString(s: string): [number, Error | null] {
    for (const char of s) {
      this.input.push(char.charCodeAt(0));
    }
    return [s.length, null];
  }

  emulate(): [number | null, EmulatorStatus] {
    while (true) {
      if (this.ip >= this.memory.length) {
        this.memory.push(...new Array(this.ip - this.memory.length + 1).fill(0));
      }
      const instruction = this.memory[this.ip];
      const opcode = instruction % 100;

      const getParameter = (offset: number): number => {
        const mode = Math.floor(instruction / 10 ** (offset + 1)) % 10;
        const param = this.memory[this.ip + offset];
        if (mode === 0) {
          if (param >= this.memory.length) {
            this.memory.push(...new Array(param - this.memory.length + 1).fill(0));
          }
          return this.memory[param];
        } else if (mode === 1) {
          return param;
        } else if (mode === 2) {
          const address = this.relativeBase + param;
          if (address >= this.memory.length) {
            this.memory.push(...new Array(address - this.memory.length + 1).fill(0));
          }
          return this.memory[address];
        } else {
          throw new Error(`Unknown parameter mode: ${mode}`);
        }
      };

      const getWriteAddress = (offset: number): number => {
        const mode = Math.floor(instruction / 10 ** (offset + 1)) % 10;
        const param = this.memory[this.ip + offset];
        let address: number;
        if (mode === 0) {
          address = param;
        } else if (mode === 2) {
          address = this.relativeBase + param;
        } else {
          throw new Error(`Invalid mode for writing: ${mode}`);
        }
        if (address >= this.memory.length) {
          this.memory.push(...new Array(address - this.memory.length + 1).fill(0));
        }
        return address;
      };

      if (opcode === 1) {
        const [a, b, c] = [getParameter(1), getParameter(2), getWriteAddress(3)];
        this.memory[c] = a + b;
        this.ip += 4;
      } else if (opcode === 2) {
        const [a, b, c] = [getParameter(1), getParameter(2), getWriteAddress(3)];
        this.memory[c] = a * b;
        this.ip += 4;
      } else if (opcode === 3) {
        if (this.input.length === 0) {
          return [null, EmulatorStatus.WAITING_FOR_INPUT];
        }
        const a = getWriteAddress(1);
        this.memory[a] = this.input.shift()!;
        this.ip += 2;
      } else if (opcode === 4) {
        const a = getParameter(1);
        this.ip += 2;
        return [a, EmulatorStatus.OUTPUT];
      } else if (opcode === 5) {
        const [a, b] = [getParameter(1), getParameter(2)];
        this.ip = a !== 0 ? b : this.ip + 3;
      } else if (opcode === 6) {
        const [a, b] = [getParameter(1), getParameter(2)];
        this.ip = a === 0 ? b : this.ip + 3;
      } else if (opcode === 7) {
        const [a, b, c] = [getParameter(1), getParameter(2), getWriteAddress(3)];
        this.memory[c] = a < b ? 1 : 0;
        this.ip += 4;
      } else if (opcode === 8) {
        const [a, b, c] = [getParameter(1), getParameter(2), getWriteAddress(3)];
        this.memory[c] = a === b ? 1 : 0;
        this.ip += 4;
      } else if (opcode === 9) {
        const a = getParameter(1);
        this.relativeBase += a;
        this.ip += 2;
      } else if (opcode === 99) {
        return [null, EmulatorStatus.HALTED];
      } else {
        throw new Error(`Unknown opcode: ${opcode} at position ${this.ip}`);
      }
    }
  }
}

function readFile(filename: string): string {
  return fs.readFileSync(filename, 'utf-8').trim();
}

function findPath(fromRoom: Room, toRoom: Room): Room[] | null {
    const queue: [Room, Room[]][] = [[fromRoom, [fromRoom]]];
    const visited = new Set<string>([fromRoom.name]);

    while (queue.length > 0) {
        const [current, path] = queue.shift()!;
        if (current === toRoom) {
            return path;
        }
        for (const neighbor of Object.values(current.connections)) {
            if (neighbor && !visited.has(neighbor.name)) {
                visited.add(neighbor.name);
                queue.push([neighbor, [...path, neighbor]]);
            }
        }
    }
    return null;
}

enum Mode {
  EXPLORE,
  NAVIGATE,
  TEST,
}
function main() {
  const text = readFile('input.txt');
  const program = text.split(',').map(Number);
  const emulator = new Emulator(program);

  const sendCommand = (formatStr: string, ...args: any[]) => {
    const cmd = formatStr.replace(/%s/g, () => args.shift());
    emulator.writeString(cmd);
  };

  const roomNameRegex = /^== (.+) ==$/;
  const listItemRegex = /^- (.+)$/;
  const takenRegex = /^You take the (.+)\.$/;
  const droppedRegex = /^You drop the (.+)\.$/;
  const resultRegex =
    /"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$/;

  const world: { [key: string]: Room } = {};
  const inventory: { [key: string]: boolean } = {};
  let mode = Mode.EXPLORE;
  let path: Room[] = [];
  let checkpoint: Room | null = null;
  let floor: Room | null = null;
  let testDir = '';
  let availableItems: string[] = [];
  let itemMask = 0;
  let last: Room | null = null;
  let lastItems: string[] = [];
  let lastDir = '';
  let outputBuilder: number[] = [];
  let currentRoom: Room | null = null;

  while (true) {
    const [char, status] = emulator.emulate();

    if (status === EmulatorStatus.HALTED) {
      const output = String.fromCharCode(...outputBuilder);
      for (const line of output.split('\n')) {
        const match = resultRegex.exec(line);
        if (match) {
          console.log(match[1]);
          return;
        }
      }
    } else if (status === EmulatorStatus.OUTPUT) {
      if (char !== null) {
        outputBuilder.push(char);
      }
    } else if (status === EmulatorStatus.WAITING_FOR_INPUT) {
      const output = String.fromCharCode(...outputBuilder);
      outputBuilder = [];

      let items: string[] = [];
      const lines = output.split('\n');
      let i = 0;
      while (i < lines.length) {
        const line = lines[i].trim();

        if (!line || line === 'Command?') {
          i += 1;
          continue;
        }

        let match = roomNameRegex.exec(line);
        if (match) {
          const name = match[1];
          i += 1;
          while (i < lines.length && lines[i].trim() !== '') {
            i += 1;
          }
          if (!world[name]) {
            currentRoom = new Room(name);
            world[name] = currentRoom;
          } else {
            currentRoom = world[name];
          }
          items = [];
          continue;
        }

        if (line === 'Doors here lead:') {
          i += 1;
          while (i < lines.length && lines[i].trim() !== '') {
            const doorLine = lines[i].trim();
            match = listItemRegex.exec(doorLine);
            if (match && currentRoom) {
              const direction = match[1];
              if (!(direction in currentRoom.connections)) {
                currentRoom.connections[direction] = null;
              }
            }
            i += 1;
          }
          continue;
        }

        if (line === 'Items here:') {
          i += 1;
          while (i < lines.length && lines[i].trim() !== '') {
            const itemLine = lines[i].trim();
            match = listItemRegex.exec(itemLine);
            if (match) {
              const item = match[1];
              items.push(item);
            }
            i += 1;
          }
          continue;
        }

        match = takenRegex.exec(line);
        if (match) {
          const taken = match[1];
          inventory[taken] = true;
          if (last) {
            currentRoom = last;
            items = lastItems.filter((item) => item !== taken);
          }
          i += 1;
          continue;
        }

        match = droppedRegex.exec(line);
        if (match) {
          const dropped = match[1];
          inventory[dropped] = false;
          if (last) {
            currentRoom = last;
            items = [...lastItems, dropped];
          }
          i += 1;
          continue;
        }

        if (line.startsWith('A loud, robotic voice says "Alert!')) {
          if (mode === Mode.EXPLORE) {
            if (path.length > 0) {
              path.pop();
            }
            checkpoint = last;
            floor = currentRoom;
            testDir = lastDir;
            if (checkpoint && testDir) {
              checkpoint.connections[testDir] = floor;
            }
          }
          [last, lastItems, lastDir] = [null, [], ''];
          i += 1;
          continue;
        }
        i += 1;
      }

      if (last !== null && lastDir && currentRoom !== null) {
        if (last.connections[lastDir] === null) {
          last.connections[lastDir] = currentRoom;
          currentRoom.connections[opposite[lastDir]] = last;
        }
      }

      [last, lastItems, lastDir] = [currentRoom, items, ''];

      if (mode === Mode.EXPLORE) {
        const blacklist = [
          'photons',
          'escape pod',
          'molten lava',
          'infinite loop',
          'giant electromagnet',
        ];
        let itemTaken = false;
        for (const item of items) {
          if (!blacklist.includes(item)) {
            sendCommand('take %s\n', item);
            itemTaken = true;
            break;
          }
        }
        if (!itemTaken) {
          let target: string | null = null;
          for (const [direction, room] of Object.entries(
            currentRoom!.connections
          )) {
            if (room === null) {
              path.push(currentRoom!);
              target = direction;
              break;
            }
          }

          if (target) {
            lastDir = target;
            sendCommand('%s\n', target);
            continue;
          }

          if (path.length > 0) {
            const lastRoom = path.pop()!;
            let backDir: string | null = null;
            for (const [direction, room] of Object.entries(
              currentRoom!.connections
            )) {
              if (room === lastRoom) {
                backDir = direction;
                break;
              }
            }
            if (backDir) {
              lastDir = backDir;
              sendCommand('%s\n', backDir);
              continue;
            } else {
              throw new Error(
                `Cannot go from "${currentRoom!.name}" to "${lastRoom.name}"`
              );
            }
          }
          if (checkpoint && floor) {
            const newPath = findPath(currentRoom!, checkpoint);
            if (newPath) {
                path = newPath.slice(1);
            }

            mode = Mode.NAVIGATE;
            continue
          }
        }
      } else if (mode === Mode.NAVIGATE) {
        if (path.length > 0) {
          const nextRoom = path.shift()!;
          let direction: string | null = null;
          for (const [dir, room] of Object.entries(currentRoom!.connections)) {
            if (room === nextRoom) {
              direction = dir;
              break;
            }
          }
          if (direction) {
            lastDir = direction;
            sendCommand('%s\n', direction);
            continue;
          } else {
            throw new Error(
              `Cannot go from "${currentRoom!.name}" to "${nextRoom.name}"`
            );
          }
        } else {
            availableItems = Object.entries(inventory)
              .filter(([, has]) => has)
              .map(([item]) => item);
            itemMask = 0;
            mode = Mode.TEST;
        }
      }
      else if (mode === Mode.TEST)
        {
            let itemActionTaken = false;
            for (const [index, item] of availableItems.entries()) {
                const targetState = (itemMask & (1 << index)) !== 0;
                if(inventory[item] !== targetState)
                {
                    const action = targetState ? "take" : "drop";
                    sendCommand("%s %s\n", action, item);
                    itemActionTaken = true;
                    break;
                }

            }
            if(!itemActionTaken)
            {
                itemMask++;
                if (testDir)
                {
                  sendCommand("%s\n", testDir);
                  continue;
                }
                else
                {
                    throw new Error("Test direction (test_dir) is not set.");
                }
            }

        }
    }
  }
}

main();
