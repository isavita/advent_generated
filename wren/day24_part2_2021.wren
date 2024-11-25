
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
            if (i % 18 == 4) {
                l.add(Num.fromString(line.split(" ")[2]))
            } else if (i % 18 == 5) {
                k.add(Num.fromString(line.split(" ")[2]))
            } else if (i % 18 == 15) {
                m.add(Num.fromString(line.split(" ")[2]))
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
        
        var min = List.filled(14, 0)
        
        for (i in 0...14) {
            if (!constraints.containsKey(i)) continue
            
            var vmin = 1
            while (vmin + constraints[i][1] < 1) {
                vmin = vmin + 1
            }
            
            min[i] = vmin
            min[constraints[i][0]] = vmin + constraints[i][1]
        }
        
        System.print(num(min))
    }
    
    static num(w) {
        var n = 0
        for (digit in w) {
            n = n * 10 + digit
        }
        return n
    }
}

Day24.solve()
