
import "io" for File

var data = File.read("input.txt")
var total = 0
var i = 0
while (i < data.count) {
    var mulPos = data.indexOf("mul(", i)
    if (mulPos == -1) break
    var p = mulPos + 4

    var num1 = 0
    var count1 = 0
    while (p < data.count && "0123456789".contains(data[p]) && count1 < 3) {
        num1 = num1 * 10 + (data[p].bytes[0] - 48)
        count1 = count1 + 1
        p = p + 1
    }
    if (count1 == 0) {
        i = mulPos + 1
        continue
    }
    if (count1 == 3 && p < data.count && "0123456789".contains(data[p])) {
        i = mulPos + 1
        continue
    }
    if (p >= data.count || data[p] != ",") {
        i = mulPos + 1
        continue
    }
    p = p + 1

    var num2 = 0
    var count2 = 0
    while (p < data.count && "0123456789".contains(data[p]) && count2 < 3) {
        num2 = num2 * 10 + (data[p].bytes[0] - 48)
        count2 = count2 + 1
        p = p + 1
    }
    if (count2 == 0) {
        i = mulPos + 1
        continue
    }
    if (count2 == 3 && p < data.count && "0123456789".contains(data[p])) {
        i = mulPos + 1
        continue
    }
    if (p >= data.count || data[p] != ")") {
        i = mulPos + 1
        continue
    }
    total = total + num1 * num2
    i = p + 1
}
System.print(total)
