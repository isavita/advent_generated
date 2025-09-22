class Main {
    static enum EmulatorStatus { HALTED, OUTPUT, WAITING_FOR_INPUT }
    static enum Mode { EXPLORE, NAVIGATE, TEST }

    static class Room {
        String name
        Map<String, Room> connections = [:]
        Room(String n) { name = n }
    }

    static class Emulator {
        List<Long> memory
        long ip = 0
        long relativeBase = 0
        Deque<Long> input = new ArrayDeque<>()

        Emulator(List<Long> program) {
            memory = new ArrayList<>(program)
        }

        long powerOf10(int exp) {
            long r = 1
            for (i in 0..<exp) r *= 10
            r
        }

        long getValue(long addr) {
            if (addr < 0) throw new RuntimeException("Negative memory address.")
            if (addr >= memory.size()) {
                def need = (int)addr - memory.size() + 1
                for (i in 0..<need) memory.add(0L)
            }
            memory[(int)addr]
        }

        void setValue(long addr, long val) {
            if (addr < 0) throw new RuntimeException("Negative memory address.")
            if (addr >= memory.size()) {
                def need = (int)addr - memory.size() + 1
                for (i in 0..<need) memory.add(0L)
            }
            memory[(int)addr] = val
        }

        long getParameter(int offset) {
            long instruction = getValue(ip)
            long mode = (instruction / powerOf10(offset + 1)) % 10
            long paramVal = getValue(ip + offset)
            if (mode == 0) return getValue(paramVal)
            if (mode == 1) return paramVal
            if (mode == 2) return getValue(relativeBase + paramVal)
            throw new RuntimeException("Unknown parameter mode.")
        }

        long getWriteAddress(int offset) {
            long instruction = getValue(ip)
            long mode = (instruction / powerOf10(offset + 1)) % 10
            long paramVal = getValue(ip + offset)
            if (mode == 0) return paramVal
            if (mode == 2) return relativeBase + paramVal
            throw new RuntimeException("Invalid mode for writing.")
        }

        void writeString(String s) {
            for (char c : s.toCharArray()) input.addLast((long)c)
        }

        List<Object> emulate() {
            while (true) {
                long opcode = getValue(ip) % 100
                switch ((int)opcode) {
                    case 1:
                        long a = getParameter(1)
                        long b = getParameter(2)
                        long c_addr = getWriteAddress(3)
                        setValue(c_addr, a + b)
                        ip += 4
                        break
                    case 2:
                        long a2 = getParameter(1)
                        long b2 = getParameter(2)
                        long c2 = getWriteAddress(3)
                        setValue(c2, a2 * b2)
                        ip += 4
                        break
                    case 3:
                        if (input.isEmpty()) return [0L, EmulatorStatus.WAITING_FOR_INPUT] as List
                        long a_addr = getWriteAddress(1)
                        setValue(a_addr, input.removeFirst())
                        ip += 2
                        break
                    case 4:
                        long out = getParameter(1)
                        ip += 2
                        return [out, EmulatorStatus.OUTPUT] as List
                    case 5:
                        long p1 = getParameter(1)
                        long p2 = getParameter(2)
                        ip = (p1 != 0) ? p2 : ip + 3
                        break
                    case 6:
                        long q1 = getParameter(1)
                        long q2 = getParameter(2)
                        ip = (q1 == 0) ? q2 : ip + 3
                        break
                    case 7:
                        long r1 = getParameter(1)
                        long r2 = getParameter(2)
                        long r3 = getWriteAddress(3)
                        setValue(r3, (r1 < r2) ? 1 : 0)
                        ip += 4
                        break
                    case 8:
                        long s1 = getParameter(1)
                        long s2 = getParameter(2)
                        long s3 = getWriteAddress(3)
                        setValue(s3, (s1 == s2) ? 1 : 0)
                        ip += 4
                        break
                    case 9:
                        long t1 = getParameter(1)
                        relativeBase += t1
                        ip += 2
                        break
                    case 99:
                        return [0L, EmulatorStatus.HALTED] as List
                    default:
                        throw new RuntimeException("Unknown opcode: " + opcode + " at position " + ip)
                }
            }
        }
    }

    static List<Long> readProgram(String filename) {
        def file = new File(filename)
        def text = file.text.trim()
        if (text.isEmpty()) return []
        text.split(',').collect { it.toLong() }
    }

    static List<Room> findPath(Room fromRoom, Room toRoom) {
        def q = new ArrayDeque()
        q.add([fromRoom, [fromRoom]])
        def visited = new HashSet<String>()
        visited << fromRoom.name

        while (!q.isEmpty()) {
            def item = q.removeFirst()
            def current = item[0]
            def path = item[1]
            if (current == toRoom) return path
            current.connections.each { dir, neighbor ->
                if (neighbor && !visited.contains(neighbor.name)) {
                    visited << neighbor.name
                    def newPath = new ArrayList(path)
                    newPath << neighbor
                    q.add([neighbor, newPath])
                }
            }
        }
        return []
    }

    static final Map<String, String> OPPOSITE = ['north':'south','south':'north','west':'east','east':'west']

    static void main(String[] args) {
        def program = readProgram("input.txt")
        def emulator = new Emulator(program)

        def roomNameRegex = ~/^== (.+) ==$/
        def listItemRegex = ~/^- (.+)$/
        def takenRegex = ~/^You take the (.+)\.$/
        def droppedRegex = ~/^You drop the (.+)\.$/
        def resultRegex = ~/^"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$/

        def itemBlacklist = ["photons","escape pod","molten lava","infinite loop","giant electromagnet"] as Set

        def roomStorage = [:]
        def inventory = [:]
        def mode = Mode.EXPLORE
        def path = []
        Room checkpoint = null
        Room floorRoom = null
        String testDir = ""
        def availableItems = []
        int itemMask = 0
        Room lastRoom = null
        def lastItems = []
        String lastDir = ""
        String outputBuilder = ""
        Room currentRoom = null

        while (true) {
            def res = emulator.emulate()
            def status = res[1]
            def val = res[0]

            if (status == EmulatorStatus.HALTED) {
                def lines = outputBuilder.split(/\r?\n/)
                for (line in lines) {
                    def m = line =~ resultRegex
                    if (m.find()) {
                        println m.group(1)
                        return
                    }
                }
                break
            } else if (status == EmulatorStatus.OUTPUT) {
                outputBuilder += (char)(val as int)
            } else if (status == EmulatorStatus.WAITING_FOR_INPUT) {
                def output = outputBuilder
                outputBuilder = ""

                def currentItemsInRoomProcessedState = []
                def lines = output.split(/\r?\n/)
                int i = 0
                boolean currentRoomUpdatedByTakenDropped = false

                while (i < lines.length) {
                    def currentLine = lines[i]

                    if (currentLine == null || currentLine.trim().isEmpty()) { i++; continue }

                    def m

                    if (currentLine == "Command?") { i++; continue }

                    m = currentLine =~ roomNameRegex
                    if (m.find()) {
                        def name = m.group(1)
                        i++
                        while (i < lines.length && lines[i].trim() != "") { i++ }
                        if (!roomStorage.containsKey(name)) {
                            roomStorage[name] = new Room(name)
                        }
                        currentRoom = roomStorage[name]
                        currentItemsInRoomProcessedState.clear()
                        // reset update flag
                        currentRoomUpdatedByTakenDropped = false
                        continue
                    }

                    if (currentLine == "Doors here lead:") {
                        i++
                        while (i < lines.length && lines[i].trim() != "") {
                            def doorLine = lines[i]
                            m = doorLine =~ listItemRegex
                            if (m.find()) {
                                def direction = m.group(1)
                                if (currentRoom && !currentRoom.connections.containsKey(direction)) {
                                    currentRoom.connections[direction] = null
                                }
                            }
                            i++
                        }
                        continue
                    }

                    if (currentLine == "Items here:") {
                        i++
                        while (i < lines.length && lines[i].trim() != "") {
                            def itemLine = lines[i]
                            m = itemLine =~ listItemRegex
                            if (m.find()) {
                                def item = m.group(1)
                                if (!currentRoomUpdatedByTakenDropped) {
                                    currentItemsInRoomProcessedState << item
                                }
                            }
                            i++
                        }
                        continue
                    }

                    m = currentLine =~ takenRegex
                    if (m.find()) {
                        def taken = m.group(1)
                        inventory[taken] = true
                        if (lastRoom) {
                            currentRoom = lastRoom
                            currentItemsInRoomProcessedState = lastItems
                            currentItemsInRoomProcessedState = currentItemsInRoomProcessedState.findAll { it != taken }
                            currentRoomUpdatedByTakenDropped = true
                        }
                        i++
                        continue
                    }

                    m = currentLine =~ droppedRegex
                    if (m.find()) {
                        def dropped = m.group(1)
                        inventory[dropped] = false
                        if (lastRoom) {
                            currentRoom = lastRoom
                            currentItemsInRoomProcessedState = lastItems
                            currentItemsInRoomProcessedState << dropped
                            currentRoomUpdatedByTakenDropped = true
                        }
                        i++
                        continue
                    }

                    if (currentLine.startsWith("A loud, robotic voice says \"Alert!")) {
                        if (mode == Mode.EXPLORE) {
                            if (!path.isEmpty()) {
                                path.remove(path.size()-1)
                            }
                            checkpoint = lastRoom
                            floorRoom = currentRoom
                            testDir = lastDir
                            if (checkpoint && testDir) {
                                checkpoint.connections[testDir] = floorRoom
                            }
                        }
                        lastRoom = null
                        lastItems.clear()
                        lastDir = ""
                        i++
                        continue
                    }

                    i++
                }

                if (lastRoom && lastDir && currentRoom) {
                    if (!lastRoom.connections.containsKey(lastDir) || lastRoom.connections[lastDir] == null) {
                        lastRoom.connections[lastDir] = currentRoom
                        currentRoom.connections[OPPOSITE[lastDir]] = lastRoom
                    }
                }

                lastRoom = currentRoom
                lastItems = currentItemsInRoomProcessedState
                lastDir = ""

                if (mode == Mode.EXPLORE) {
                    boolean itemActionTaken = false
                    for (String item : currentItemsInRoomProcessedState) {
                        if (!itemBlacklist.contains(item)) {
                            emulator.writeString("take " + item + "\n")
                            itemActionTaken = true
                            break
                        }
                    }
                    if (itemActionTaken) {
                        continue
                    }

                    String targetDirection = ""
                    for (entry in currentRoom?.connections?.entrySet()) {
                        def dir = entry.key
                        def roomPtr = entry.value
                        if (roomPtr == null) {
                            path << currentRoom
                            targetDirection = dir
                            break
                        }
                    }

                    if (targetDirection) {
                        lastDir = targetDirection
                        emulator.writeString(targetDirection + "\n")
                        continue
                    }

                    if (!path.isEmpty()) {
                        def last_p_room = path.remove(path.size()-1)
                        String back_dir = ""
                        for (entry in currentRoom?.connections?.entrySet()) {
                            def dir = entry.key
                            def roomPtr = entry.value
                            if (roomPtr == last_p_room) {
                                back_dir = dir
                                break
                            }
                        }
                        if (back_dir) {
                            lastDir = back_dir
                            emulator.writeString(back_dir + "\n")
                            continue
                        } else {
                            throw new RuntimeException("Cannot go from \"" + (currentRoom?.name) + "\" to \"" + (last_p_room?.name) + "\"")
                        }
                    }

                    if (checkpoint && floorRoom) {
                        def new_path = findPath(currentRoom, checkpoint)
                        if (new_path && !new_path.isEmpty()) {
                            path = new_path.subList(1, new_path.size())
                        }
                        mode = Mode.NAVIGATE
                        continue
                    }

                } else if (mode == Mode.NAVIGATE) {
                    if (!path.isEmpty()) {
                        def next_room = path.get(0)
                        path.remove(0)

                        String direction = ""
                        for (entry in currentRoom?.connections?.entrySet()) {
                            def dir = entry.key
                            def roomPtr = entry.value
                            if (roomPtr == next_room) {
                                direction = dir
                                break
                            }
                        }
                        if (direction) {
                            lastDir = direction
                            emulator.writeString(direction + "\n")
                            continue
                        } else {
                            throw new RuntimeException("Cannot go from \"" + (currentRoom?.name) + "\" to \"" + (next_room?.name) + "\"")
                        }
                    } else {
                        availableItems = inventory.findAll { it.value }.collect { it.key }.sort()
                        itemMask = 0
                        mode = Mode.TEST
                        continue
                    }
                } else if (mode == Mode.TEST) {
                    boolean itemActionTaken = false
                    for (int index = 0; index < availableItems.size(); index++) {
                        def item = availableItems[index]
                        def targetState = ((itemMask >> index) & 1) != 0
                        if ((inventory[item] != targetState)) {
                            def action = targetState ? "take" : "drop"
                            emulator.writeString(action + " " + item + "\n")
                            itemActionTaken = true
                            break
                        }
                    }

                    if (itemActionTaken) {
                        continue
                    } else {
                        itemMask++
                        if (testDir) {
                            emulator.writeString(testDir + "\n")
                            continue
                        } else {
                            throw new RuntimeException("Test direction (test_dir) is not set.")
                        }
                    }
                }
            }
        }
    }
}

Main.main(new String[0])