
const fs = require('node:fs');

class Room {
  constructor(name) {
    this.name = name;
    this.connections = {};
  }
}

const opposite = {
  north: 'south',
  south: 'north',
  west: 'east',
  east: 'west',
};

const EmulatorStatus = {
  HALTED: 0,
  OUTPUT: 1,
  WAITING_FOR_INPUT: 2,
};

class Emulator {
  constructor(program, inputValues = []) {
    this.memory = [...program];
    this.input = [...inputValues];
    this.ip = 0;
    this.relativeBase = 0;
  }

  writeString(s) {
    for (const char of s) {
      this.input.push(char.charCodeAt(0));
    }
    return s.length;
  }

  emulate() {
    while (true) {
      if (this.ip >= this.memory.length) {
        this.memory.length = Math.max(this.memory.length, this.ip + 1)
        this.memory.fill(0,this.memory.length - (this.ip + 1) + this.ip +1, this.ip +1)
      }
      const instruction = this.memory[this.ip];
      const opcode = instruction % 100;

      const getParameter = (offset) => {
        const mode = Math.floor(instruction / (10 ** (offset + 1))) % 10;
        const param = this.memory[this.ip + offset];
        if (mode === 0) {
          
          this.memory.length = Math.max(this.memory.length, param + 1)
          this.memory.fill(0, this.memory.length- (param + 1) +param+1,param +1 )
          return this.memory[param];
        } else if (mode === 1) {
          return param;
        } else if (mode === 2) {
          const address = this.relativeBase + param;
            this.memory.length = Math.max(this.memory.length, address + 1)
            this.memory.fill(0, this.memory.length-(address + 1)+address +1,address+1)
          return this.memory[address];
        } else {
          throw new Error(`Unknown parameter mode: ${mode}`);
        }
      };

      const getWriteAddress = (offset) => {
        const mode = Math.floor(instruction / (10 ** (offset + 1))) % 10;
        const param = this.memory[this.ip + offset];
        let address
        if (mode === 0) {
          address = param;
        } else if (mode === 2) {
          address = this.relativeBase + param;
        } else {
          throw new Error(`Invalid mode for writing: ${mode}`);
        }
            this.memory.length = Math.max(this.memory.length, address + 1)
            this.memory.fill(0,this.memory.length-(address+1) + address+1,address+1)
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
        this.memory[a] = this.input.shift();
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

function findPath(fromRoom, toRoom) {
  const queue = [[fromRoom, [fromRoom]]];
  const visited = new Set([fromRoom.name]);

  while (queue.length > 0) {
    const [current, path] = queue.shift();
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

const readFile = (filename) => fs.readFileSync(filename, 'utf8').trim();
const toInt64 = (s) => parseInt(s, 10);

function main() {
  const text = readFile('input.txt');
  const program = text.split(',').map(toInt64);
  const emulator = new Emulator(program);

  const sendCommand = (formatStr, ...args) => {
    const cmd = args.reduce((str, arg, i) => str.replace(`%${i}`, arg), formatStr)
    
    emulator.writeString(cmd);
  };

  const roomNameRegex = /^== (.+) ==$/;
  const listItemRegex = /^- (.+)$/;
  const takenRegex = /^You take the (.+)\.$/;
  const droppedRegex = /^You drop the (.+)\.$/;
  const resultRegex = /"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$/;

  const Mode = { EXPLORE: 0, NAVIGATE: 1, TEST: 2 };
  const world = {};
  const inventory = {};
  let mode = Mode.EXPLORE;
  let path = [];
  let checkpoint = null;
  let floor = null;
  let testDir = '';
  let availableItems = [];
  let itemMask = 0;
  let last = null;
  let lastItems = [];
  let lastDir = '';
  let outputBuilder = [];
  let currentRoom = null;

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

      let items = [];
      const lines = output.split('\n');
      let i = 0;
      while (i < lines.length) {
        const line = lines[i].trim();

        if (!line || line === 'Command?') {
          i++;
          continue;
        }

        let match = roomNameRegex.exec(line);
        if (match) {
          const name = match[1];
          i++;
          while (i < lines.length && lines[i].trim() !== '') {
            i++;
          }
          currentRoom = world[name] || (world[name] = new Room(name));
          items = [];
          continue;
        }

        if (line === 'Doors here lead:') {
          i++;
          while (i < lines.length && lines[i].trim() !== '') {
            const doorLine = lines[i].trim();
            match = listItemRegex.exec(doorLine);
            if (match && currentRoom) {
              const direction = match[1];
              currentRoom.connections[direction] = currentRoom.connections[direction] || null;
            }
            i++;
          }
          continue;
        }

        if (line === 'Items here:') {
          i++;
          while (i < lines.length && lines[i].trim() !== '') {
            const itemLine = lines[i].trim();
            match = listItemRegex.exec(itemLine);
            if (match) {
              items.push(match[1]);
            }
            i++;
          }
          continue;
        }
        
        match = takenRegex.exec(line)
        if(match){
          const taken = match[1]
          inventory[taken] = true;
          if(last){
            currentRoom = last
            items = lastItems.filter(item => item !== taken)
          }
          i++;
          continue;
        }

        match = droppedRegex.exec(line);
        if(match){
            const dropped = match[1];
            inventory[dropped] = false;
            if(last){
                currentRoom = last;
                items = [...lastItems, dropped];
            }
            i++
            continue
        }

        if (line.startsWith('A loud, robotic voice says "Alert!')) {
          if (mode === Mode.EXPLORE) {
            if (path.length > 0) {
              path.pop();
            }
            [checkpoint, floor, testDir] = [last, currentRoom, lastDir];
            if (checkpoint && testDir) {
              checkpoint.connections[testDir] = floor;
            }
          }
          [last, lastItems, lastDir] = [null, [], ''];
          i++;
          continue;
        }

        i++;
      }

      if (last !== null && lastDir && currentRoom !== null) {
        if (last.connections[lastDir] === undefined || last.connections[lastDir] === null) {
          last.connections[lastDir] = currentRoom;
          currentRoom.connections[opposite[lastDir]] = last;
        }
      }

      [last, lastItems, lastDir] = [currentRoom, items, ''];

      if (mode === Mode.EXPLORE) {
        const blacklist = ['photons', 'escape pod', 'molten lava', 'infinite loop', 'giant electromagnet'];
        if (items.some(item => !blacklist.includes(item))) {
          for (const item of items) {
            if (!blacklist.includes(item)) {
               sendCommand('take %0\n', item);
               break
            }
          }
        }
        else {
          let target = null;
          for (const direction in currentRoom.connections) {
            if (currentRoom.connections[direction] === null) {
              path.push(currentRoom);
              target = direction;
              break;
            }
          }

          if (target) {
            lastDir = target;
            sendCommand('%0\n', target);
          } else if (path.length > 0) {
            const lastRoom = path.pop();
            let backDir = null;
            for (const direction in currentRoom.connections) {
              if (currentRoom.connections[direction] === lastRoom) {
                backDir = direction;
                break;
              }
            }
            if (backDir) {
              lastDir = backDir;
              sendCommand('%0\n', backDir);
            } else {
              throw new Error(`Cannot go from "${currentRoom.name}" to "${lastRoom.name}"`);
            }
          }
          else if (checkpoint && floor)
          {
            const newPath = findPath(currentRoom, checkpoint);
                if (newPath) {
                    path = newPath.slice(1);
                }
            mode = Mode.NAVIGATE;
          }

        }
      } else if (mode === Mode.NAVIGATE) {
        if (path.length > 0) {
          const nextRoom = path.shift();
          let direction = null;
          for (const dir in currentRoom.connections) {
            if (currentRoom.connections[dir] === nextRoom) {
              direction = dir;
              break;
            }
          }
          if (direction) {
            lastDir = direction;
            sendCommand('%0\n', direction);
          } else {
            throw new Error(`Cannot go from "${currentRoom.name}" to "${nextRoom.name}"`);
          }
        } else {
          availableItems = Object.keys(inventory).filter((item) => inventory[item]);
          itemMask = 0;
          mode = Mode.TEST;
        }
      } else if (mode === Mode.TEST) {
        if(availableItems.length > 0){
          for (let index = 0; index < availableItems.length; index++) {
            const item = availableItems[index];
            const targetState = (itemMask & (1 << index)) !== 0;
            if (inventory[item] !== targetState) {
              const action = targetState ? 'take' : 'drop';
              sendCommand('%0 %1\n', action, item);
              break;
            }
            if(index === availableItems.length -1){
              itemMask++;
              if (testDir) {
                sendCommand('%0\n', testDir);
              } else {
                throw new Error('Test direction (test_dir) is not set.');
              }
            }
          }
        }
        else{
            itemMask++;
            if (testDir) {
                sendCommand('%0\n', testDir);
            } else {
                throw new Error('Test direction (test_dir) is not set.');
            }
        }
      }
    }
  }
}

main();
