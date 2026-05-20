
import "io" for File

class Solver {
  static solve(target, current, nums, idx) {
    if (idx == nums.count) return current == target
    if (current > target) return false
    var val = nums[idx]
    return solve(target, current + val, nums, idx + 1) || solve(target, current * val, nums, idx + 1)
  }
}

var content = File.read("input.txt")
var lines = content.split("\n")
var total = 0
for (line in lines) {
  if (line.count == 0) continue
  var parts = line.split(":")
  if (parts.count < 2) continue
  var target = Num.fromString(parts[0])
  var nums = []
  var curr = ""
  for (i in 0...parts[1].count) {
    var c = parts[1][i]
    if (c == " " || c == "\r" || c == "\n") {
      if (curr != "") {
        nums.add(Num.fromString(curr))
        curr = ""
      }
    } else {
      curr = curr + c
    }
  }
  if (curr != "") nums.add(Num.fromString(curr))
  
  if (nums.count > 0 && Solver.solve(target, nums[0], nums, 1)) {
    total = total + target
  }
}
System.print(total)
