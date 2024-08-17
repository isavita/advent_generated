import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")

  for (i in 0..lines.count-2) {
    for (j in i+1..lines.count-1) {
      var diff = 0
      for (k in 0..lines[i].count-1) {
        if (lines[i][k] != lines[j][k]) {
          diff = diff + 1
          if (diff > 1) break
        }
      }
      if (diff == 1) {
        var common = ""
        for (k in 0..lines[i].count-1) {
          if (lines[i][k] == lines[j][k]) {
            common = common + lines[i][k]
          }
        }
        System.print(common)
        return
      }
    }
  }
}

main.call()