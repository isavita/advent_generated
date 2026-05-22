
import "io" for File

var s = File.read("input.txt").trim()
var n = 14
var ans = -1
if (s.count >= n) {
  for (i in n..s.count) {
    var seen = {}
    var unique = true
    for (char in s[i-n...i]) {
      if (seen[char]) {
        unique = false
        break
      }
      seen[char] = true
    }
    if (unique) {
      ans = i
      break
    }
  }
}
System.print(ans)
