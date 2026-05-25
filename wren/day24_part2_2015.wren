import "io" for File

class Solution {
    static run() {
        var content = File.read("input.txt")
        var weights = []
        for (line in content.split("\n")) {
            var trimmed = line.trim()
            if (trimmed.bytes.count > 0) {
                weights.add(Num.fromString(trimmed))
            }
        }

        var n = weights.count
        for (i in 0...(n - 1)) {
            for (j in (i + 1)...n) {
                if (weights[j] > weights[i]) {
                    var temp = weights[i]
                    weights[i] = weights[j]
                    weights[j] = temp
                }
            }
        }

        var total = 0
        for (w in weights) total = total + w

        if (total % 4 != 0) {
            System.print("Cannot balance")
            return
        }

        var target = total / 4
        var bestProd = -1

        var solve
        solve = Fn.new {|start_idx, count_left, sum_left, prod|
            if (sum_left == 0 && count_left == 0) {
                if (bestProd == -1 || prod < bestProd) {
                    bestProd = prod
                }
                return true
            }
            if (count_left == 0 || sum_left < 0 || start_idx >= n) {
                return false
            }

            var found = false
            for (i in start_idx...n) {
                var w = weights[i]
                if (w > sum_left) continue
                if (solve.call(i + 1, count_left - 1, sum_left - w, prod * w)) {
                    found = true
                }
            }
            return found
        }

        for (k in 1..n) {
            if (solve.call(0, k, target, 1)) {
                System.print(bestProd)
                return
            }
        }
    }
}

Solution.run()