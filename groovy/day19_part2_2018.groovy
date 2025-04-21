
import java.util.regex.Matcher
import java.util.regex.Pattern

class Solution {

    // Define operations using closures
    static final Map<String, Closure> instructions = [
        addr: { List<Integer> r, int a, int b -> r[a] + r[b] },
        addi: { List<Integer> r, int a, int b -> r[a] + b },
        mulr: { List<Integer> r, int a, int b -> r[a] * r[b] },
        muli: { List<Integer> r, int a, int b -> r[a] * b },
        banr: { List<Integer> r, int a, int b -> r[a] & r[b] },
        bani: { List<Integer> r, int a, int b -> r[a] & b },
        borr: { List<Integer> r, int a, int b -> r[a] | r[b] },
        bori: { List<Integer> r, int a, int b -> r[a] | b },
        setr: { List<Integer> r, int a, int b -> r[a] },
        seti: { List<Integer> r, int a, int b -> a },
        gtir: { List<Integer> r, int a, int b -> a > r[b] ? 1 : 0 },
        gtri: { List<Integer> r, int a, int b -> r[a] > b ? 1 : 0 },
        gtrr: { List<Integer> r, int a, int b -> r[a] > r[b] ? 1 : 0 },
        eqir: { List<Integer> r, int a, int b -> a == r[b] ? 1 : 0 },
        eqri: { List<Integer> r, int a, int b -> r[a] == b ? 1 : 0 },
        eqrr: { List<Integer> r, int a, int b -> r[a] == r[b] ? 1 : 0 }
    ]

    static Tuple loadProgram(List<String> lines) {
        List<Closure> program = []
        int ipRegister = -1
        Pattern numPattern = Pattern.compile(/\d+/)

        lines.each { line ->
            if (line.startsWith("#ip")) {
                ipRegister = line.split()[1].toInteger()
            } else {
                List<String> parts = line.split()
                String opName = parts[0]
                Closure op = instructions[opName]

                Matcher matcher = numPattern.matcher(line)
                List<Integer> nums = []
                while (matcher.find()) {
                    nums.add(matcher.group().toInteger())
                }
                int a = nums[0]
                int b = nums[1]
                int c = nums[2]

                program.add({ List<Integer> r ->
                    r[c] = op(r, a, b)
                })
            }
        }
        return new Tuple(ipRegister, program)
    }

    static List<Integer> runProgram(int ipRegister, List<Closure> program, List<Integer> registers, int maxCycles) {
        int ip = 0
        int cycles = 0

        while (ip >= 0 && ip < program.size()) {
            registers[ipRegister] = ip
            program[ip](registers)
            ip = registers[ipRegister] + 1
            cycles++
            if (maxCycles > 0 && cycles >= maxCycles) {
                break
            }
        }
        return registers
    }
    
    // Optimized sum of divisors calculation
    static int sumOfDivisors(int n) {
        int total = 0
        int limit = Math.sqrt(n) as int
        (1..limit).each { i ->
            if (n % i == 0) {
                total += i
                if (i * i != n) { 
                    total += n / i
                }
            }
        }
        return total
    }

    static void main(String[] args) {
        List<String> lines = new File("input.txt").readLines().findAll { it.trim() }

        def (ipRegister, program) = loadProgram(lines)

        List<Integer> registers = [0] * 6 
        registers[0] = 1 // Start with register 0 set to 1

        // Run for a limited number of cycles to find the target number
        registers = runProgram(ipRegister, program, registers, 1000) 

        // The target number is likely the largest value in registers after stabilization
        int n = registers.max() 

        // The program effectively calculates the sum of divisors of n
        int total = sumOfDivisors(n)
        
        println total
    }
}

// Execute the main method
Solution.main(null)
