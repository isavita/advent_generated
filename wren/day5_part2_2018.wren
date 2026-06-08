import "io" for File

var react = Fn.new { |bytes, skip|
    var stack = []
    for (b in bytes) {
        // Check if this unit type should be skipped (case insensitive)
        if (skip != -1 && (b | 32) == skip) continue
        
        if (stack.count > 0 && (stack[-1] ^ b) == 32) {
            stack.removeAt(stack.count - 1)
        } else {
            stack.add(b)
        }
    }
    return stack.count
}

var input = File.read("input.txt").trim()
var bytes = input.bytes.toList // Convert to list of byte values (integers)

var minLength = input.count
for (c in 97..122) { // 'a' to 'z'
    var len = react.call(bytes, c)
    if (len < minLength) minLength = len
}

System.print(minLength)