
import 'dart:io';

class Room {
  String name;
  Map<String, Room?> connections = {};
  Room(this.name);
}

enum Mode { Explore, Navigate, Test }

Map<String, String> opposite = {
  "north": "south",
  "south": "north",
  "west": "east",
  "east": "west"
};

void main() {
  String text = File("input.txt").readAsStringSync().trim();

  List<int> program = text.split(",").map(int.parse).toList();

  Emulator emulator = Emulator(program);

  void sendCommand(String format, [List<dynamic>? args]) {
    String cmd = format;
    if (args != null) {
      cmd = cmd.replaceAllMapped(RegExp(r'{}'), (match) {
        return args.removeAt(0).toString();
      });
    }
    emulator.writeString(cmd);
  }

  RegExp roomNameRegex = RegExp(r'^== (.+) ==$');
  RegExp listItemRegex = RegExp(r'^- (.+)$');
  RegExp takenRegex = RegExp(r'^You take the (.+)\.$');
  RegExp droppedRegex = RegExp(r'^You drop the (.+)\.$');

  Map<String, Room> world = {};
  Map<String, bool> inventory = {};

  Mode mode = Mode.Explore;
  List<Room> path = [];
  Room? checkpoint, floor;
  String? testDir;

  List<String> availableItems = [];
  int itemMask = 0;

  Room? last;
  List<String>? lastItems;
  String? lastDir;

  StringBuffer outputBuilder = StringBuffer();

  loop:
  while (true) {
    EmulatorResult result = emulate(emulator);
    switch (result.status) {
      case EmulatorStatus.Halted:
        String output = outputBuilder.toString();
        outputBuilder.clear();

        String? resultValue;

        RegExp resultRegex = RegExp(
            r'"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$');

        for (String line in output.split("\n")) {
          Match? match = resultRegex.firstMatch(line);
          if (match != null) {
            resultValue = match.group(1);
          }
        }

        print(resultValue);
        return;

      case EmulatorStatus.Output:
        outputBuilder.write(String.fromCharCode(result.value));
        break;

      case EmulatorStatus.WaitingForInput:
        String output = outputBuilder.toString();
        outputBuilder.clear();

        Room? current;
        List<String> items = [];

        List<String> lines = output.split("\n");
        for (int i = 0; i < lines.length; i++) {
          String line = lines[i];

          if (line == "" || line == "Command?") {
            continue;
          }

          Match? match = roomNameRegex.firstMatch(line);
          if (match != null) {
            String name = match.group(1)!;

            List<String> description = [];
            while (i + 1 < lines.length && lines[i + 1] != "") {
              description.add(lines[i + 1]);
              i++;
            }

            current = world[name];
            if (current == null) {
              current = Room(name);
              world[name] = current;
            }

            items = [];

            continue;
          }

          if (line == "Doors here lead:") {
            bool fresh = (current!.connections.isEmpty);

            if (fresh) {
              current.connections = {};
            }

            while (i + 1 < lines.length && lines[i + 1] != "") {
              Match match = listItemRegex.firstMatch(lines[i + 1])!;
              String direction = match.group(1)!;
              if (fresh) {
                current.connections[direction] = null;
              }
              i++;
            }

            continue;
          }

          if (line == "Items here:") {
            while (i + 1 < lines.length && lines[i + 1] != "") {
              Match match = listItemRegex.firstMatch(lines[i + 1])!;
              String item = match.group(1)!;
              items.add(item);
              i++;
            }

            continue;
          }

          match = takenRegex.firstMatch(line);
          if (match != null) {
            String taken = match.group(1)!;
            inventory[taken] = true;

            current = last;
            for (String item in lastItems!) {
              if (item != taken) {
                items.add(item);
              }
            }

            continue;
          }

          match = droppedRegex.firstMatch(line);
          if (match != null) {
            String dropped = match.group(1)!;
            inventory[dropped] = false;

            current = last;
            items = [...lastItems!, dropped];

            continue;
          }

          if (line.startsWith(r'A loud, robotic voice says "Alert!')) {
            if (mode == Mode.Explore) {
              path = path.sublist(0, path.length - 1);
              checkpoint = last;
              floor = current;
              testDir = lastDir;
              checkpoint!.connections[testDir!] = floor;
            }

            last = null;
            lastItems = null;
            lastDir = null;

            continue;
          }

          throw line;
        }

        if (last != null &&
            lastDir != null &&
            last!.connections[lastDir] == null) {
          last!.connections[lastDir] = current;
          current!.connections[opposite[lastDir]!] = last;
        }

        last = current;
        lastItems = items;
        lastDir = null;

        switch (mode) {
          case Mode.Explore:
            List<String> blacklist = [
              "photons",
              "escape pod",
              "molten lava",
              "infinite loop",
              "giant electromagnet",
            ];

            itemLoop:
            for (String item in items) {
              for (String bad in blacklist) {
                if (item == bad) {
                  continue itemLoop;
                }
              }

              sendCommand("take {}\n", [item]);
              continue loop;
            }

            String? target;
            for (MapEntry<String, Room?> entry
                in current!.connections.entries) {
              if (entry.value == null) {
                path.add(current);
                target = entry.key;
                break;
              }
            }

            if (target == null && path.isNotEmpty) {
              Room lastRoom = path.last;
              for (MapEntry<String, Room?> entry
                  in current.connections.entries) {
                if (entry.value == lastRoom) {
                  path.removeLast();
                  target = entry.key;
                  break;
                }
              }
              if (target == null) {
                throw 'cannot go from "${current.name}" to "${lastRoom.name}"';
              }
            }

            if (target != null) {
              lastDir = target;
              sendCommand("{}\n", [target]);
              continue loop;
            }

            path = findPath(current, checkpoint!)!.sublist(1);
            mode = Mode.Navigate;
            continue caseModeNavigate;

          caseModeNavigate:
          case Mode.Navigate:
            if (path.isNotEmpty) {
              for (MapEntry<String, Room?> entry
                  in current!.connections.entries) {
                if (entry.value == path[0]) {
                  path.removeAt(0);
                  sendCommand("{}\n", [entry.key]);
                  continue loop;
                }
              }

              throw 'cannot go from "${current.name}" to "${path[0].name}"';
            }

            availableItems = [];
            for (String item in inventory.keys) {
              availableItems.add(item);
            }
            itemMask = 0;
            mode = Mode.Test;
            continue caseModeTest;

          caseModeTest:
          case Mode.Test:
            for (int index = 0; index < availableItems.length; index++) {
              String item = availableItems[index];
              bool targetState = (itemMask & (1 << index)) != 0;
              if (inventory[item] != targetState) {
                String action = targetState ? "take" : "drop";
                sendCommand("{} {}\n", [action, item]);
                continue loop;
              }
            }

            itemMask++;
            sendCommand("{}\n", [testDir]);
            continue loop;
        }
    }
  }
}

List<Room>? findPath(Room from, Room to) {
  List<MapEntry<Room, List<Room>>> queue = [
    MapEntry(from, [from])
  ];

  Map<Room, bool> visited = {from: true};

  while (queue.isNotEmpty) {
    MapEntry<Room, List<Room>> item = queue.removeAt(0);

    if (item.key == to) {
      return item.value;
    }

    for (Room? next in item.key.connections.values) {
      if (next != null && visited[next] != true) {
        visited[next] = true;
        List<Room> path = [...item.value, next];
        queue.add(MapEntry(next, path));
      }
    }
  }

  return null;
}

enum EmulatorStatus { Halted, Output, WaitingForInput }

class EmulatorResult {
  int value;
  EmulatorStatus status;
  EmulatorResult(this.value, this.status);
}

class Emulator {
  List<int> memory;
  List<int> input = [];
  int ip = 0;
  int relativeBase = 0;

  Emulator(List<int> program) : memory = [...program];

  void writeString(String s) {
    for (int char in s.codeUnits) {
      input.add(char);
    }
  }

  EmulatorResult emulate([List<int>? newInput]) {
    if (newInput != null) {
      input.addAll(newInput);
    }

    int getMemoryValue(int index) {
      if (index >= memory.length) {
        memory.addAll(List.filled(index - memory.length + 1, 0));
      }
      return memory[index];
    }

    void setMemoryValue(int index, int value) {
      if (index >= memory.length) {
        memory.addAll(List.filled(index - memory.length + 1, 0));
      }
      memory[index] = value;
    }

    int getParameterValue(int offset) {
      int parameter = getMemoryValue(ip + offset);
      int mode = memory[ip] ~/ pow(10, offset + 1) % 10;
      switch (mode) {
        case 0:
          return getMemoryValue(parameter);
        case 1:
          return parameter;
        case 2:
          return getMemoryValue(relativeBase + parameter);
        default:
          throw 'fault: invalid parameter mode: ip=$ip instruction=${memory[ip]} offset=$offset mode=$mode';
      }
    }
    
    void setParameterValue(int offset, int value) {
      int parameter = getMemoryValue(ip + offset);
      int mode = memory[ip] ~/ pow(10, offset + 1) % 10;
      switch (mode) {
        case 0:
          setMemoryValue(parameter, value);
          break;
        case 1:
          throw 'fault: immediate mode cannot be used for write: ip=$ip instruction=${memory[ip]} offset=$offset';
        case 2:
          setMemoryValue(relativeBase + parameter, value);
          break;
        default:
          throw 'fault: invalid parameter mode: ip=$ip instruction=${memory[ip]} offset=$offset mode=$mode';
      }
    }

    while (true) {
      int instruction = memory[ip];
      int opcode = instruction % 100;

      switch (opcode) {
        case 1:
          setParameterValue(3, getParameterValue(1) + getParameterValue(2));
          ip += 4;
          break;

        case 2:
          setParameterValue(3, getParameterValue(1) * getParameterValue(2));
          ip += 4;
          break;

        case 3:
          if (input.isEmpty) {
            return EmulatorResult(0, EmulatorStatus.WaitingForInput);
          }
          setParameterValue(1, input.removeAt(0));
          ip += 2;
          break;

        case 4:
          int outputValue = getParameterValue(1);
          ip += 2;
          return EmulatorResult(outputValue, EmulatorStatus.Output);

        case 5:
          if (getParameterValue(1) != 0) {
            ip = getParameterValue(2);
          } else {
            ip += 3;
          }
          break;

        case 6:
          if (getParameterValue(1) == 0) {
            ip = getParameterValue(2);
          } else {
            ip += 3;
          }
          break;

        case 7:
          setParameterValue(3, getParameterValue(1) < getParameterValue(2) ? 1 : 0);
          ip += 4;
          break;

        case 8:
          setParameterValue(3, getParameterValue(1) == getParameterValue(2) ? 1 : 0);
          ip += 4;
          break;

        case 9:
          relativeBase += getParameterValue(1);
          ip += 2;
          break;

        case 99:
          return EmulatorResult(0, EmulatorStatus.Halted);

        default:
          throw 'fault: invalid opcode: ip=$ip instruction=$instruction opcode=$opcode';
      }
    }
  }
}

EmulatorResult emulate(Emulator emulator, [List<int>? input]) {
  return emulator.emulate(input);
}

int pow(int base, int exponent) {
  int result = 1;
  while (exponent > 0) {
    if (exponent & 1 != 0) {
      result *= base;
    }
    exponent >>= 1;
    base *= base;
  }
  return result;
}
