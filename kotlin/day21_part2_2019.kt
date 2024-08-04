import java.io.File
import java.io.FileNotFoundException
import kotlin.math.pow
import kotlin.math.roundToInt

class VM {
    var code = mutableMapOf<Int, Int>()
    var ip = 0
    var input = mutableListOf<Int>()
    var output = mutableListOf<Int>()
    var relativeBase = 0

    fun load(filename: String) {
        val content = File(filename).readText()
        val listStr = content.trim().split(",")

        for (i in listStr.indices) {
            code[i] = listStr[i].toInt()
        }
    }

    fun run() {
        var arity: Int

        while (true) {
            val cmd = Cmd(code[ip]!!)
            when (cmd.opCode()) {
                1 -> {
                    arity = 3
                    val params = getParamsAddresses(ip, cmd, arity)
                    code[params[2]] = (code[params[0]] ?: 0) + (code[params[1]] ?: 0)
                }
                2 -> {
                    arity = 3
                    val params = getParamsAddresses(ip, cmd, arity)
                    code[params[2]] = (code[params[0]] ?: 0) * (code[params[1]] ?: 0)
                }
                3 -> {
                    arity = 1
                    val params = getParamsAddresses(ip, cmd, arity)
                    code[params[0]] = input.removeAt(0)
                }
                4 -> {
                    arity = 1
                    val params = getParamsAddresses(ip, cmd, arity)
                    output.add(code[params[0]] ?: 0)
                }
                5 -> {
                    arity = 2
                    val params = getParamsAddresses(ip, cmd, arity)
                    if ((code[params[0]] ?: 0) != 0) {
                        ip = code[params[1]] ?: 0
                        continue
                    }
                }
                6 -> {
                    arity = 2
                    val params = getParamsAddresses(ip, cmd, arity)
                    if ((code[params[0]] ?: 0) == 0) {
                        ip = code[params[1]] ?: 0
                        continue
                    }
                }
                7 -> {
                    arity = 3
                    val params = getParamsAddresses(ip, cmd, arity)

                    code[params[2]] = if ((code[params[0]] ?: 0) < (code[params[1]] ?: 0)) 1 else 0
                }
                8 -> {
                    arity = 3
                    val params = getParamsAddresses(ip, cmd, arity)

                    code[params[2]] = if ((code[params[0]] ?: 0) == (code[params[1]] ?: 0)) 1 else 0
                }
                9 -> {
                    arity = 1
                    val params = getParamsAddresses(ip, cmd, arity)
                    relativeBase += code[params[0]] ?: 0
                }
                99 -> {
                    return
                }
                else -> {
                    throw Exception("not an opcode $cmd")
                }
            }

            ip += arity + 1
        }
    }

    fun getParamsAddresses(pos: Int, cmd: Cmd, arity: Int): List<Int> {
        val modes = cmd.modes(arity)
        val results = mutableListOf<Int>()

        for (i in 0 until arity) {
            results.add(getParamAddress(pos + i + 1, modes[i]))
        }

        return results
    }

    fun getParamAddress(pos: Int, mode: Mode): Int {
        return when (mode) {
            Mode.Position -> code[pos] ?: 0
            Mode.Immediate -> pos
            Mode.Relative -> relativeBase + (code[pos] ?: 0)
        }
    }
}

class Cmd(private val value: Int) {
    fun opCode(): Int {
        return value % 100
    }

    fun modes(arity: Int): List<Mode> {
        val modes = mutableListOf<Mode>()

        val valueModes = (value / 100).toString()
        var j = valueModes.length - 1
        repeat(arity) {
            modes.add(
                when (if (j < 0) 0 else Character.getNumericValue(valueModes[j])) {
                    0 -> Mode.Position
                    1 -> Mode.Immediate
                    2 -> Mode.Relative
                    else -> throw Exception("Invalid mode")
                }
            )
            j--
        }

        return modes
    }
}

enum class Mode {
    Position,
    Immediate,
    Relative
}

fun main() {
    val vm = VM()
    vm.load("input.txt")

    val instructions = listOf(
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN"
    )

    instructions.forEach {
        it.forEach { char ->
            vm.input.add(char.toInt())
        }
        vm.input.add('\n'.toInt())
    }

    vm.run()
    println(vm.output)
}