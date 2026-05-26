
import "io" for File

class Solution {
  static run() {
    var shapes = [
      [[0, 0], [1, 0], [2, 0], [3, 0]],
      [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],
      [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],
      [[0, 0], [0, 1], [0, 2], [0, 3]],
      [[0, 0], [1, 0], [0, 1], [1, 1]]
    ]

    var jetPattern = File.read("input.txt").trim()
    var jetLen = jetPattern.count
    var jetIndex = 0

    var chamber = [127]
    var highestY = 0

    var canMove = Fn.new { |shape, px, py|
      for (pt in shape) {
        var nx = px + pt[0]
        var ny = py + pt[1]
        if (nx < 0 || nx >= 7 || ny <= 0) return false
        if (ny < chamber.count) {
          if ((chamber[ny] & (1 << nx)) != 0) return false
        }
      }
      return true
    }

    for (rockNum in 0...2022) {
      var shape = shapes[rockNum % 5]
      var px = 2
      var py = highestY + 4
      
      while (true) {
        var jetDir = jetPattern[jetIndex % jetLen]
        jetIndex = jetIndex + 1
        
        var pushDx = (jetDir == ">") ? 1 : -1
        if (canMove.call(shape, px + pushDx, py)) {
          px = px + pushDx
        }
        
        if (canMove.call(shape, px, py - 1)) {
          py = py - 1
        } else {
          for (pt in shape) {
            var nx = px + pt[0]
            var ny = py + pt[1]
            while (chamber.count <= ny) {
              chamber.add(0)
            }
            chamber[ny] = chamber[ny] | (1 << nx)
            if (ny > highestY) {
              highestY = ny
            }
          }
          break
        }
      }
    }

    System.print(highestY)
  }
}

Solution.run()
