
import "io" for File

class Solution {
  static run() {
    var content = File.read("input.txt").replace("\n", "").replace("\r", "")
    var code = content.split(",").map {|x| Num.fromString(x) }.toList
    var sol = Solution.new(code)
    sol.solve()
  }

  construct new(code) {
    _code = code
    _maxOutput = 0
  }

  solve() {
    var phases = [0, 1, 2, 3, 4]
    permute(phases, 0, 4)
    System.print(_maxOutput)
  }

  permute(arr, k, n) {
    if (k == n) {
      testPhase(arr)
    } else {
      for (i in k..n) {
        swap(arr, k, i)
        permute(arr, k + 1, n)
        swap(arr, k, i)
      }
    }
  }

  swap(arr, i, j) {
    var tmp = arr[i]
    arr[i] = arr[j]
    arr[j] = tmp
  }

  testPhase(phase) {
    var signal = 0
    for (i in 0..4) {
      signal = runVM([phase[i], signal])
    }
    if (signal > _maxOutput) {
      _maxOutput = signal
    }
  }

  runVM(inputs) {
    var mem = _code.toList
    var ip = 0
    var inputIdx = 0
    var outputVal = null

    while (ip >= 0 && ip < mem.count) {
      var cmd = mem[ip]
      var opcode = cmd % 100
      var m1 = ((cmd / 100).floor) % 10
      var m2 = ((cmd / 1000).floor) % 10

      var getParam = Fn.new {|offset, mode|
        var val = mem[ip + offset]
        return (mode == 1) ? val : mem[val]
      }

      if (opcode == 1) {
        var p1 = getParam.call(1, m1)
        var p2 = getParam.call(2, m2)
        var addr = mem[ip + 3]
        mem[addr] = p1 + p2
        ip = ip + 4
      } else if (opcode == 2) {
        var p1 = getParam.call(1, m1)
        var p2 = getParam.call(2, m2)
        var addr = mem[ip + 3]
        mem[addr] = p1 * p2
        ip = ip + 4
      } else if (opcode == 3) {
        var addr = mem[ip + 1]
        mem[addr] = inputs[inputIdx]
        inputIdx = inputIdx + 1
        ip = ip + 2
      } else if (opcode == 4) {
        outputVal = getParam.call(1, m1)
        ip = ip + 2
      } else if (opcode == 5) {
        var p1 = getParam.call(1, m1)
        var p2 = getParam.call(2, m2)
        ip = (p1 != 0) ? p2 : ip + 3
      } else if (opcode == 6) {
        var p1 = getParam.call(1, m1)
        var p2 = getParam.call(2, m2)
        ip = (p1 == 0) ? p2 : ip + 3
      } else if (opcode == 7) {
        var p1 = getParam.call(1, m1)
        var p2 = getParam.call(2, m2)
        var addr = mem[ip + 3]
        mem[addr] = (p1 < p2) ? 1 : 0
        ip = ip + 4
      } else if (opcode == 8) {
        var p1 = getParam.call(1, m1)
        var p2 = getParam.call(2, m2)
        var addr = mem[ip + 3]
        mem[addr] = (p1 == p2) ? 1 : 0
        ip = ip + 4
      } else if (opcode == 99) {
        return outputVal
      }
    }
    return outputVal
  }
}

Solution.run()
