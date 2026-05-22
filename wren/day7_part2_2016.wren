
import "io" for File

var content = File.read("input.txt")
var count = 0
for (line in content.split("\n")) {
  line = line.trim()
  if (line == "") continue

  var parts = line.split("[")
  var supernets = [parts[0]]
  var hypernets = []
  if (parts.count > 1) {
    for (i in 1...parts.count) {
      var sub = parts[i].split("]")
      hypernets.add(sub[0])
      if (sub.count > 1) supernets.add(sub[1])
    }
  }

  var ssl = false
  for (sn in supernets) {
    if (sn.count >= 3) {
      for (i in 0...(sn.count - 2)) {
        var a = sn[i]
        var b = sn[i+1]
        var c = sn[i+2]
        if (a == c && a != b) {
          var bab = b + a + b
          for (hn in hypernets) {
            if (hn.contains(bab)) {
              ssl = true
              break
            }
          }
        }
        if (ssl) break
      }
    }
    if (ssl) break
  }
  if (ssl) count = count + 1
}
System.print(count)
