
import "io" for File

class Solver {
  static isValid(p) {
    var straight = false
    for (i in 0...(p.count - 2)) {
      if (p[i] + 1 == p[i+1] && p[i+1] + 1 == p[i+2]) {
        straight = true
        break
      }
    }
    if (!straight) return false

    for (c in p) {
      if (c == 105 || c == 111 || c == 108) return false
    }

    var pairs = 0
    var i = 0
    while (i < p.count - 1) {
      if (p[i] == p[i+1]) {
        pairs = pairs + 1
        i = i + 2
      } else {
        i = i + 1
      }
    }
    return pairs >= 2
  }

  static increment(p) {
    var i = p.count - 1
    while (i >= 0) {
      p[i] = p[i] + 1
      if (p[i] == 105 || p[i] == 111 || p[i] == 108) {
        p[i] = p[i] + 1
      }
      if (p[i] > 122) {
        p[i] = 97
        i = i - 1
      } else {
        break
      }
    }
  }

  static findNext(p) {
    while (true) {
      increment(p)
      if (isValid(p)) return
    }
  }

  static run() {
    var password = File.read("input.txt").trim()
    var p = password.bytes.toList
    findNext(p)
    findNext(p)
    System.print(p.map { |x| String.fromCodePoint(x) }.join(""))
  }
}

Solver.run()
