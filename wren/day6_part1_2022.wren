
import "io" for File

var s = File.read("input.txt").trim()
for (i in 0...(s.count - 3)) {
  var a = s[i]
  var b = s[i+1]
  var c = s[i+2]
  var d = s[i+3]
  if (a != b && a != c && a != d && b != c && b != d && c != d) {
    System.print(i + 4)
    break
  }
}
