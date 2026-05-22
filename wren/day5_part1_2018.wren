
import "io" for File

var content = File.read("input.txt").trim()
var stack = []
for (b in content.bytes) {
  if (!stack.isEmpty && (stack[-1] - b).abs == 32) {
    stack.removeAt(-1)
  } else {
    stack.add(b)
  }
}
System.print(stack.count)
