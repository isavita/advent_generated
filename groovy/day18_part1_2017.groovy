
import java.nio.file.Files
import java.nio.file.Paths

class Duet {

    static void main(String[] args) {
        def instructions = Files.readAllLines(Paths.get("input.txt"))
        
        def registers = [:]
        def lastSound = 0
        def ip = 0

        while (ip >= 0 && ip < instructions.size()) {
            def instruction = instructions[ip].split(" ")
            def cmd = instruction[0]
            def x = instruction[1]
            def y = instruction.length > 2 ? instruction[2] : null
            
            def xVal = isRegister(x) ? (registers[x] ?: 0) : x.toLong()
            def yVal = y ? (isRegister(y) ? (registers[y] ?: 0) : y.toLong()) : null

            switch (cmd) {
                case "snd":
                    lastSound = xVal
                    ip++
                    break
                case "set":
                    registers[x] = yVal
                    ip++
                    break
                case "add":
                    registers[x] = (registers[x] ?: 0) + yVal
                    ip++
                    break
                case "mul":
                    registers[x] = (registers[x] ?: 0) * yVal
                    ip++
                    break
                case "mod":
                    registers[x] = (registers[x] ?: 0) % yVal
                    ip++
                    break
                case "rcv":
                    if (xVal != 0) {
                        println lastSound
                        return  // Exit immediately after the first recovery.
                    }
                    ip++
                    break
                case "jgz":
                    if (xVal > 0) {
                        ip += yVal
                    } else {
                        ip++
                    }
                    break
            }
        }
    }

    static boolean isRegister(String s) {
        return s.length() == 1 && s.matches("[a-z]")
    }
}
