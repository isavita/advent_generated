
import "io" for File

var content = File.read("input.txt")
var sum = 0
var i = 0
var len = content.bytes.count
var bytes = content.bytes
while (i < len) {
    var b = bytes[i]
    if (b == 45 && i + 1 < len && bytes[i+1] >= 48 && bytes[i+1] <= 57) {
        i = i + 1
        var num = 0
        while (i < len && bytes[i] >= 48 && bytes[i] <= 57) {
            num = num * 10 + (bytes[i] - 48)
            i = i + 1
        }
        sum = sum - num
    } else if (b >= 48 && b <= 57) {
        var num = 0
        while (i < len && bytes[i] >= 48 && bytes[i] <= 57) {
            num = num * 10 + (bytes[i] - 48)
            i = i + 1
        }
        sum = sum + num
    } else {
        i = i + 1
    }
}
System.print(sum)
