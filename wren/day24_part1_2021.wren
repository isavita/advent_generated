
import "io" for File
import "os" for Process

class Day24 {
    static solve() {
        var input = File.read("input.txt").trim()
        var lines = input.split("\n")
        
        var k = []
        var l = []
        var m = []
        
        for (i in 0...lines.count) {
            var line = lines[i]
            var parts = line.split(" ")
            
            if (i % 18 == 4) {
                l.add(Num.fromString(parts[2]))
            } else if (i % 18 == 5) {
                k.add(Num.fromString(parts[2]))
            } else if (i % 18 == 15) {
                m.add(Num.fromString(parts[2]))
            }
        }
        
        var constraints = {}
        var stack = []
        
        for (i in 0...l.count) {
            if (l[i] == 1) {
                stack.add(i)
            } else if (l[i] == 26) {
                var pop = stack[-1]
                stack.removeAt(-1)
                constraints[pop] = [i, m[pop] + k[i]]
            }
        }
        
        var max = List.filled(14, 0)
        
        for (i in 0...14) {
            if (!constraints.containsKey(i)) continue
            
            var vmax = 9
            while (vmax + constraints[i][1] > 9) {
                vmax = vmax - 1
            }
            
            max[i] = vmax
            max[constraints[i][0]] = vmax + constraints[i][1]
        }
        
        var result = 0
        for (digit in max) {
            result = result * 10 + digit
        }
        
        System.print(result)
    }
}

Day24.solve()
