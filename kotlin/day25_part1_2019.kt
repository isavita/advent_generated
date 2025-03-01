
import java.io.File
import java.util.*
import java.util.regex.Pattern

class Room(val name: String) {
    val connections: MutableMap<String, Room?> = mutableMapOf()
}

object Mode {
    const val EXPLORE = 0
    const val NAVIGATE = 1
    const val TEST = 2
}

val opposite = mapOf("north" to "south", "south" to "north", "west" to "east", "east" to "west")

object EmulatorStatus {
    const val HALTED = 0
    const val OUTPUT = 1
    const val WAITING_FOR_INPUT = 2
}

class Emulator(program: List<Long>) {
    private val memory = program.toMutableList()
    private val input = mutableListOf<Long>()
    private var ip = 0
    private var relativeBase = 0

    fun writeString(s: String) {
        s.forEach { input.add(it.code.toLong()) }
    }

    fun emulate(): Pair<Long?, Int> {
        while (true) {
            if (ip >= memory.size) {
                memory.addAll(List(ip - memory.size + 1) { 0L })
            }
            val instruction = memory[ip]
            val opcode = instruction % 100

            fun getParameter(offset: Int): Long {
                val mode = (instruction / (10.0.pow(offset + 1).toInt())) % 10
                val param = memory[ip + offset]
                return when (mode) {
                    0L -> memory.getOrElse(param.toInt()) { 0L }
                    1L -> param
                    2L -> memory.getOrElse((relativeBase + param).toInt()) { 0L }
                    else -> throw Exception("Unknown parameter mode: $mode")
                }
            }

            fun getWriteAddress(offset: Int): Int {
                val mode = (instruction / (10.0.pow(offset + 1).toInt())) % 10
                val param = memory[ip + offset]
                val address = when (mode) {
                    0L -> param.toInt()
                    2L -> (relativeBase + param).toInt()
                    else -> throw Exception("Invalid mode for writing: $mode")
                }
                if (address >= memory.size) {
                    memory.addAll(List(address - memory.size + 1) { 0L })
                }
                return address
            }

            when (opcode) {
                1L -> {
                    val (a, b, c) = Triple(getParameter(1), getParameter(2), getWriteAddress(3))
                    memory[c] = a + b
                    ip += 4
                }
                2L -> {
                    val (a, b, c) = Triple(getParameter(1), getParameter(2), getWriteAddress(3))
                    memory[c] = a * b
                    ip += 4
                }
                3L -> {
                    if (input.isEmpty()) {
                        return null to EmulatorStatus.WAITING_FOR_INPUT
                    }
                    memory[getWriteAddress(1)] = input.removeAt(0)
                    ip += 2
                }
                4L -> {
                    val a = getParameter(1)
                    ip += 2
                    return a to EmulatorStatus.OUTPUT
                }
                5L -> {
                    val (a, b) = Pair(getParameter(1), getParameter(2))
                    ip = if (a != 0L) b.toInt() else ip + 3
                }
                6L -> {
                    val (a, b) = Pair(getParameter(1), getParameter(2))
                    ip = if (a == 0L) b.toInt() else ip + 3
                }
                7L -> {
                    val (a, b, c) = Triple(getParameter(1), getParameter(2), getWriteAddress(3))
                    memory[c] = if (a < b) 1L else 0L
                    ip += 4
                }
                8L -> {
                    val (a, b, c) = Triple(getParameter(1), getParameter(2), getWriteAddress(3))
                    memory[c] = if (a == b) 1L else 0L
                    ip += 4
                }
                9L -> {
                    relativeBase += getParameter(1).toInt()
                    ip += 2
                }
                99L -> return null to EmulatorStatus.HALTED
                else -> throw Exception("Unknown opcode: $opcode at position $ip")
            }
        }
    }
    private fun Double.pow(exponent: Int): Long {
        return Math.pow(this, exponent.toDouble()).toLong()
    }
}

fun findPath(fromRoom: Room, toRoom: Room): List<Room>? {
    val queue = LinkedList<Pair<Room, List<Room>>>()
    queue.add(fromRoom to listOf(fromRoom))
    val visited = mutableSetOf(fromRoom.name)

    while (queue.isNotEmpty()) {
        val (current, path) = queue.poll()
        if (current == toRoom) {
            return path
        }
        for (neighbor in current.connections.values) {
            if (neighbor != null && neighbor.name !in visited) {
                visited.add(neighbor.name)
                queue.add(neighbor to path + neighbor)
            }
        }
    }
    return null
}

fun main() {
    val text = File("input.txt").readText().trim()
    val program = text.split(",").map { it.toLong() }
    val emulator = Emulator(program)

    fun sendCommand(formatStr: String, vararg args: Any) {
        val cmd = String.format(formatStr, *args)
        emulator.writeString(cmd)
    }

    val roomNameRegex = Pattern.compile("^== (.+) ==$")
    val listItemRegex = Pattern.compile("^- (.+)$")
    val takenRegex = Pattern.compile("^You take the (.+)\\.$")
    val droppedRegex = Pattern.compile("^You drop the (.+)\\.$")
    val resultRegex = Pattern.compile("\"Oh, hello! You should be able to get in by typing (\\d+) on the keypad at the main airlock\\.\"$")

    val world: MutableMap<String, Room> = mutableMapOf()
    val inventory: MutableMap<String, Boolean> = mutableMapOf()
    var mode = Mode.EXPLORE
    var path: List<Room> = emptyList()
    var checkpoint: Room? = null
    var floor: Room? = null
    var testDir: String = ""
    var availableItems: List<String> = emptyList()
    var itemMask = 0
    var last: Room? = null
    var lastItems: List<String> = emptyList()
    var lastDir: String = ""
    val outputBuilder = mutableListOf<Long>()
    var currentRoom: Room? = null

    while (true) {
        val (char, status) = emulator.emulate()

        when (status) {
            EmulatorStatus.HALTED -> {
                val output = outputBuilder.map { it.toInt().toChar() }.joinToString("")
                output.split("\n").forEach { line ->
                    val match = resultRegex.matcher(line)
                    if (match.find()) {
                        println(match.group(1))
                        return
                    }
                }
            }
            EmulatorStatus.OUTPUT -> if (char != null) outputBuilder.add(char)
            EmulatorStatus.WAITING_FOR_INPUT -> {
                val output = outputBuilder.map { it.toInt().toChar() }.joinToString("")
                outputBuilder.clear()

                var items: List<String> = emptyList()
                val lines = output.split("\n")
                var i = 0
                while (i < lines.size) {
                    val line = lines[i].trim()

                    if (line.isEmpty() || line == "Command?") {
                        i++
                        continue
                    }

                    var match = roomNameRegex.matcher(line)
                    if (match.find()) {
                        val name = match.group(1)
                        i++
                        while (i < lines.size && lines[i].trim().isNotEmpty()) {
                            i++
                        }
                        currentRoom = world.getOrPut(name) { Room(name) }
                        items = emptyList()
                        continue
                    }

                    if (line == "Doors here lead:") {
                        i++
                        while (i < lines.size && lines[i].trim().isNotEmpty()) {
                            val doorLine = lines[i].trim()
                            match = listItemRegex.matcher(doorLine)
                            if (match.find() && currentRoom != null) {
                                val direction = match.group(1)
                                currentRoom.connections.putIfAbsent(direction, null)
                            }
                            i++
                        }
                        continue
                    }

                    if (line == "Items here:") {
                        i++
                        while (i < lines.size && lines[i].trim().isNotEmpty()) {
                            val itemLine = lines[i].trim()
                            match = listItemRegex.matcher(itemLine)
                            if (match.find()) {
                                items = items + match.group(1)
                            }
                            i++
                        }
                        continue
                    }

                    match = takenRegex.matcher(line)
                    if (match.find()) {
                        val taken = match.group(1)
                        inventory[taken] = true
                        if (last != null) {
                            currentRoom = last
                            items = lastItems.filter { it != taken }
                        }
                        i++
                        continue
                    }

                    match = droppedRegex.matcher(line)
                    if (match.find()) {
                        val dropped = match.group(1)
                        inventory[dropped] = false
                        if (last != null) {
                            currentRoom = last
                            items = lastItems + dropped
                        }
                        i++
                        continue
                    }
                    if (line.startsWith("A loud, robotic voice says \"Alert!")) {
                        if (mode == Mode.EXPLORE) {
                            if (path.isNotEmpty()) path = path.dropLast(1)

                            checkpoint = last
                            floor = currentRoom
                            testDir = lastDir

                            if(checkpoint != null && testDir.isNotEmpty()){
                                checkpoint.connections[testDir] = floor
                            }
                        }
                         last = null
                         lastItems = emptyList()
                         lastDir = ""
                         i++
                         continue
                    }

                    i++
                }

                if (last != null && lastDir.isNotEmpty() && currentRoom != null) {
                    if (last.connections.get(lastDir) == null){
                        last.connections[lastDir] = currentRoom
                        currentRoom.connections[opposite[lastDir]!!] = last
                    }
                }

                last = currentRoom
                lastItems = items
                lastDir = ""

                when (mode) {
                    Mode.EXPLORE -> {
                        val blacklist = listOf("photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet")
                        var itemTaken = false
                        for (item in items) {
                            if (item !in blacklist) {
                                sendCommand("take %s\n", item)
                                itemTaken = true
                                break
                            }
                        }
                        if (!itemTaken) {
                            var target: String? = null
                            for ((direction, room) in currentRoom!!.connections) {
                                if (room == null) {
                                    path = path + currentRoom
                                    target = direction
                                    break
                                }
                            }

                            if (target != null) {
                                lastDir = target
                                sendCommand("%s\n", target)
                            } else {
                                if (path.isNotEmpty()) {
                                    val lastRoom = path.last()
                                    path = path.dropLast(1)
                                    var backDir: String? = null
                                    for ((direction, room) in currentRoom!!.connections) {
                                        if (room == lastRoom) {
                                            backDir = direction
                                            break
                                        }
                                    }
                                    if (backDir != null) {
                                        lastDir = backDir
                                        sendCommand("%s\n", backDir)
                                    } else {
                                        throw Exception("Cannot go from \"${currentRoom.name}\" to \"${lastRoom.name}\"")
                                    }
                                } else {
                                    if (checkpoint != null && floor != null) {
                                          val newPath = findPath(currentRoom, checkpoint)
                                        if(newPath != null){
                                            path = newPath.drop(1)
                                        }
                                        mode = Mode.NAVIGATE
                                    }
                                }
                            }
                        }
                    }
                    Mode.NAVIGATE -> {
                        if (path.isNotEmpty()) {
                            val nextRoom = path.first()
                            path = path.drop(1)
                            var direction: String? = null
                            for ((dir, room) in currentRoom!!.connections) {
                                if (room == nextRoom) {
                                    direction = dir
                                    break
                                }
                            }
                            if (direction != null) {
                                lastDir = direction
                                sendCommand("%s\n", direction)
                            } else {
                                throw Exception("Cannot go from \"${currentRoom.name}\" to \"${nextRoom.name}\"")
                            }
                        } else {
                            availableItems = inventory.filter { it.value }.keys.toList()
                            itemMask = 0
                            mode = Mode.TEST
                        }
                    }
                    Mode.TEST -> {
                        var itemProcessed = false
                        for ((index, item) in availableItems.withIndex()) {
                            val targetState = (itemMask and (1 shl index)) != 0
                            if (inventory[item] != targetState) {
                                val action = if (targetState) "take" else "drop"
                                sendCommand("%s %s\n", action, item)
                                itemProcessed = true
                                break
                            }
                        }
                        if (!itemProcessed) {
                            itemMask++
                            if (testDir.isNotEmpty()) {
                                sendCommand("%s\n", testDir)
                            }
                            else{
                                throw Exception("Test direction (test_dir) is not set")
                            }
                        }
                    }
                }
            }
        }
    }
}
