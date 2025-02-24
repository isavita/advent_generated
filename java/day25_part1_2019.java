
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    static class Room {
        String name;
        Map<String, Room> connections = new HashMap<>();

        Room(String name) {
            this.name = name;
        }
    }

    enum Mode {
        EXPLORE,
        NAVIGATE,
        TEST
    }

    enum EmulatorStatus {
        HALTED,
        OUTPUT,
        WAITING_FOR_INPUT
    }

    static final Map<String, String> opposite = Map.of(
            "north", "south",
            "south", "north",
            "west", "east",
            "east", "west"
    );

    static class Emulator {
        long[] memory;
        List<Long> input;
        int ip = 0;
        long relativeBase = 0;

        Emulator(List<Long> program) {
            memory = program.stream().mapToLong(Long::longValue).toArray();
            input = new ArrayList<>();
        }

        void writeString(String s) {
            for (char c : s.toCharArray()) {
                input.add((long) c);
            }
        }

        EmulatorStatus emulate(StringBuilder outputBuilder) {
            while (true) {
                ensureCapacity(ip);
                long instruction = memory[ip];
                long opcode = instruction % 100;

                if (opcode == 99) {
                    return EmulatorStatus.HALTED;
                }

                switch ((int) opcode) {
                    case 1: {
                        long a = getParameter(1, instruction);
                        long b = getParameter(2, instruction);
                        int c = (int) getWriteAddress(3, instruction);
                        memory[c] = a + b;
                        ip += 4;
                        break;
                    }
                    case 2: {
                        long a = getParameter(1, instruction);
                        long b = getParameter(2, instruction);
                        int c = (int) getWriteAddress(3, instruction);
                        memory[c] = a * b;
                        ip += 4;
                        break;
                    }
                    case 3: {
                        if (input.isEmpty()) {
                            return EmulatorStatus.WAITING_FOR_INPUT;
                        }
                        int a = (int) getWriteAddress(1, instruction);
                        memory[a] = input.remove(0);
                        ip += 2;
                        break;
                    }
                    case 4: {
                        long a = getParameter(1, instruction);
                        outputBuilder.append((char) a);
                        ip += 2;
                        return EmulatorStatus.OUTPUT;
                    }
                    case 5: {
                        long a = getParameter(1, instruction);
                        long b = getParameter(2, instruction);
                        ip = (a != 0) ? (int) b : ip + 3;
                        break;
                    }
                    case 6: {
                        long a = getParameter(1, instruction);
                        long b = getParameter(2, instruction);
                        ip = (a == 0) ? (int) b : ip + 3;
                        break;
                    }
                    case 7: {
                        long a = getParameter(1, instruction);
                        long b = getParameter(2, instruction);
                        int c = (int) getWriteAddress(3, instruction);
                        memory[c] = (a < b) ? 1 : 0;
                        ip += 4;
                        break;
                    }
                    case 8: {
                        long a = getParameter(1, instruction);
                        long b = getParameter(2, instruction);
                        int c = (int) getWriteAddress(3, instruction);
                        memory[c] = (a == b) ? 1 : 0;
                        ip += 4;
                        break;
                    }
                    case 9: {
                        long a = getParameter(1, instruction);
                        relativeBase += a;
                        ip += 2;
                        break;
                    }
                    default:
                        throw new IllegalArgumentException("Unknown opcode: " + opcode + " at position " + ip);
                }
            }
        }

        private long getParameter(int offset, long instruction) {
            int mode = (int) ((instruction / (long) Math.pow(10, offset + 1)) % 10);
            ensureCapacity(ip + offset);
            long param = memory[ip + offset];
            return switch (mode) {
                case 0 -> {
                    ensureCapacity((int) param);
                    yield memory[(int) param];
                }
                case 1 -> param;
                case 2 -> {
                    int address = (int) (relativeBase + param);
                    ensureCapacity(address);
                    yield memory[address];
                }
                default -> throw new IllegalArgumentException("Unknown parameter mode: " + mode);
            };
        }

        private long getWriteAddress(int offset, long instruction) {
            int mode = (int) ((instruction / (long) Math.pow(10, offset + 1)) % 10);
            ensureCapacity(ip + offset);
            long param = memory[ip + offset];
            int address = switch (mode) {
                case 0 -> (int) param;
                case 2 -> (int) (relativeBase + param);
                default -> throw new IllegalArgumentException("Invalid mode for writing: " + mode);
            };
            ensureCapacity(address);
            return address;
        }

        private void ensureCapacity(int index) {
            if (index >= memory.length) {
                memory = Arrays.copyOf(memory, index + 1);
            }
        }
    }

    public static void main(String[] args) throws IOException {
        String text = readFile("input.txt");
        List<Long> program = Arrays.stream(text.split(",")).map(Long::parseLong).toList();
        Emulator emulator = new Emulator(program);

        Pattern roomNameRegex = Pattern.compile("^== (.+) ==$");
        Pattern listItemRegex = Pattern.compile("^- (.+)$");
        Pattern takenRegex = Pattern.compile("^You take the (.+)\\.$");
        Pattern droppedRegex = Pattern.compile("^You drop the (.+)\\.$");
        Pattern resultRegex = Pattern.compile("\"Oh, hello! You should be able to get in by typing (\\d+) on the keypad at the main airlock\\.\"$");

        Map<String, Room> world = new HashMap<>();
        Map<String, Boolean> inventory = new HashMap<>();
        Mode mode = Mode.EXPLORE;
        Deque<Room> path = new ArrayDeque<>();
        Room checkpoint = null;
        Room floor = null;
        String testDir = "";
        List<String> availableItems = new ArrayList<>();
        int itemMask = 0;
        Room last = null;
        List<String> lastItems = new ArrayList<>();
        String lastDir = "";
        StringBuilder outputBuilder = new StringBuilder();
        Room currentRoom = null;

        while (true) {
            EmulatorStatus status = emulator.emulate(outputBuilder);

            if (status == EmulatorStatus.HALTED) {
                String output = outputBuilder.toString();
                String[] lines = output.split("\n");
                for (String line : lines) {
                    Matcher match = resultRegex.matcher(line);
                    if (match.find()) {
                        System.out.println(match.group(1));
                        return;
                    }
                }
            } else if (status == EmulatorStatus.WAITING_FOR_INPUT) {
                String output = outputBuilder.toString();
                outputBuilder.setLength(0);

                List<String> items = new ArrayList<>();
                String[] lines = output.split("\n");
                int i = 0;
                while (i < lines.length) {
                    String line = lines[i].trim();

                    if (line.isEmpty() || line.equals("Command?")) {
                        i++;
                        continue;
                    }

                    Matcher roomMatch = roomNameRegex.matcher(line);
                    if (roomMatch.find()) {
                        String name = roomMatch.group(1);
                        i++;
                        while (i < lines.length && !lines[i].trim().isEmpty()) {
                            i++;
                        }
                        currentRoom = world.computeIfAbsent(name, Room::new);
                        items.clear();
                        continue;
                    }

                    if (line.equals("Doors here lead:")) {
                        i++;
                        while (i < lines.length && !lines[i].trim().isEmpty()) {
                            String doorLine = lines[i].trim();
                            Matcher doorMatch = listItemRegex.matcher(doorLine);
                            if (doorMatch.find() && currentRoom != null) {
                                String direction = doorMatch.group(1);
                                currentRoom.connections.putIfAbsent(direction, null);
                            }
                            i++;
                        }
                        continue;
                    }

                    if (line.equals("Items here:")) {
                        i++;
                        while (i < lines.length && !lines[i].trim().isEmpty()) {
                            String itemLine = lines[i].trim();
                            Matcher itemMatch = listItemRegex.matcher(itemLine);
                            if (itemMatch.find()) {
                                String item = itemMatch.group(1);
                                items.add(item);
                            }
                            i++;
                        }
                        continue;
                    }

                    Matcher takenMatch = takenRegex.matcher(line);
                    if (takenMatch.find()) {
                        String taken = takenMatch.group(1);
                        inventory.put(taken, true);
                        if (last != null) {
                            currentRoom = last;
                            items = new ArrayList<>(lastItems);
                            items.remove(taken);
                        }
                        i++;
                        continue;
                    }

                    Matcher droppedMatch = droppedRegex.matcher(line);
                    if (droppedMatch.find()) {
                        String dropped = droppedMatch.group(1);
                        inventory.put(dropped, false);
                        if (last != null) {
                            currentRoom = last;
                            items = new ArrayList<>(lastItems);
                            items.add(dropped);
                        }
                        i++;
                        continue;
                    }

                    if (line.startsWith("A loud, robotic voice says \"Alert!")) {
                        if (mode == Mode.EXPLORE) {
                            if (!path.isEmpty()) {
                                path.removeLast();
                            }
                            checkpoint = last;
                            floor = currentRoom;
                            testDir = lastDir;
                            if (checkpoint != null && !testDir.isEmpty()) {
                                checkpoint.connections.put(testDir, floor);
                            }
                        }
                        last = null;
                        lastItems.clear();
                        lastDir = "";
                        i++;
                        continue;
                    }

                    i++;
                }

                if (last != null && !lastDir.isEmpty() && currentRoom != null) {
                    last.connections.putIfAbsent(lastDir, currentRoom);
                    currentRoom.connections.putIfAbsent(opposite.get(lastDir), last);
                }

                last = currentRoom;
                lastItems = items;
                lastDir = "";

                if (mode == Mode.EXPLORE) {
                    List<String> blacklist = List.of("photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet");
                    boolean tookItem = false;
                    for (String item : items) {
                        if (!blacklist.contains(item)) {
                            send_command(emulator, "take " + item + "\n");
                            tookItem = true;
                            break;
                        }
                    }

                    if (!tookItem) {
                        String target = null;
                        for (Map.Entry<String, Room> entry : currentRoom.connections.entrySet()) {
                            if (entry.getValue() == null) {
                                path.addLast(currentRoom);
                                target = entry.getKey();
                                break;
                            }
                        }

                        if (target != null) {
                            lastDir = target;
                            send_command(emulator, target + "\n");
                            continue;
                        }

                        if (!path.isEmpty()) {
                            Room lastRoom = path.removeLast();
                            String backDir = null;
                            for (Map.Entry<String, Room> entry : currentRoom.connections.entrySet()) {
                                if (entry.getValue() == lastRoom) {
                                    backDir = entry.getKey();
                                    break;
                                }
                            }
                            if (backDir != null) {
                                lastDir = backDir;
                                send_command(emulator, backDir + "\n");
                                continue;
                            } else {
                                throw new IllegalStateException("Cannot go from \"" + currentRoom.name + "\" to \"" + lastRoom.name + "\"");
                            }
                        }

                        if (checkpoint != null && floor != null) {
                            List<Room> newPath = findPath(currentRoom, checkpoint);
                            if (newPath != null) {
                                path.addAll(newPath.subList(1, newPath.size()));
                            }
                            mode = Mode.NAVIGATE;
                            continue;
                        }
                    }
                } else if (mode == Mode.NAVIGATE) {
                    if (!path.isEmpty()) {
                        Room nextRoom = path.removeFirst();
                        String direction = null;
                        for (Map.Entry<String, Room> entry : currentRoom.connections.entrySet()) {
                            if (entry.getValue() == nextRoom) {
                                direction = entry.getKey();
                                break;
                            }
                        }
                        if (direction != null) {
                            lastDir = direction;
                            send_command(emulator, direction + "\n");
                            continue;
                        } else {
                            throw new IllegalStateException("Cannot go from \"" + currentRoom.name + "\" to \"" + nextRoom.name + "\"");
                        }
                    } else {
                        availableItems = inventory.entrySet().stream().filter(Map.Entry::getValue).map(Map.Entry::getKey).toList();
                        itemMask = 0;
                        mode = Mode.TEST;
                    }
                } else if (mode == Mode.TEST) {
                    boolean itemChanged = false;
                    for (int index = 0; index < availableItems.size(); index++) {
                        String item = availableItems.get(index);
                        boolean targetState = (itemMask & (1 << index)) != 0;
                        if (inventory.getOrDefault(item, false) != targetState) {
                            String action = targetState ? "take" : "drop";
                            send_command(emulator, action + " " + item + "\n");
                            itemChanged = true;
                            break;
                        }
                    }
                    if (!itemChanged) {
                        itemMask++;
                        if (!testDir.isEmpty()) {
                            send_command(emulator, testDir + "\n");
                        } else {
                            throw new IllegalStateException("Test direction (testDir) is not set.");
                        }
                    }
                    continue;
                }
            }
        }
    }

    private static void send_command(Emulator emulator, String command) {
        emulator.writeString(command);
    }

    private static List<Room> findPath(Room fromRoom, Room toRoom) {
        Queue<Pair<Room, List<Room>>> queue = new LinkedList<>();
        queue.offer(new Pair<>(fromRoom, List.of(fromRoom)));
        Set<String> visited = new HashSet<>();
        visited.add(fromRoom.name);

        while (!queue.isEmpty()) {
            Pair<Room, List<Room>> currentPair = queue.poll();
            Room current = currentPair.first;
            List<Room> path = currentPair.second;

            if (current == toRoom) {
                return path;
            }

            for (Room neighbor : current.connections.values()) {
                if (neighbor != null && !visited.contains(neighbor.name)) {
                    visited.add(neighbor.name);
                    List<Room> newPath = new ArrayList<>(path);
                    newPath.add(neighbor);
                    queue.offer(new Pair<>(neighbor, newPath));
                }
            }
        }
        return null;
    }

    private static String readFile(String filename) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            return reader.readLine().trim();
        }
    }

    static class Pair<T, U> {
        public final T first;
        public final U second;

        public Pair(T first, U second) {
            this.first = first;
            this.second = second;
        }
    }
}
