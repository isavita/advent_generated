
import "io" for File

var addStr = Fn.new { |a, b|
    var i = a.count - 1
    var j = b.count - 1
    var carr = 0
    var res = []
    var ab = a.bytes
    var bb = b.bytes
    while (i >= 0 || j >= 0 || carr > 0) {
        var da = 0
        if (i >= 0) {
            da = ab[i] - 48
            i = i - 1
        }
        var db = 0
        if (j >= 0) {
            db = bb[j] - 48
            j = j - 1
        }
        var sum = da + db + carr
        res.add(sum % 10)
        carr = (sum / 10).floor
    }
    var s = ""
    var k = res.count - 1
    while (k >= 0) {
        s = s + res[k].toString
        k = k - 1
    }
    return s
}

var mulStr = Fn.new { |a, b|
    if (a == "0" || b == "0") return "0"
    var la = a.count
    var lb = b.count
    var tmp = List.filled(la + lb, 0)
    var ab = a.bytes
    var bb = b.bytes
    for (i in 0...la) {
        var da = ab[la - 1 - i] - 48
        for (j in 0...lb) {
            var db = bb[lb - 1 - j] - 48
            tmp[i + j] = tmp[i + j] + da * db
        }
    }
    var carr = 0
    var tmpLen = tmp.count
    for (k in 0...tmpLen) {
        var sum = tmp[k] + carr
        tmp[k] = sum % 10
        carr = (sum / 10).floor
    }
    var idx = tmpLen - 1
    while (idx > 0 && tmp[idx] == 0) {
        idx = idx - 1
    }
    var res = ""
    while (idx >= 0) {
        res = res + tmp[idx].toString
        idx = idx - 1
    }
    return res
}

var lines = File.read("input.txt").split("\n")
if (lines.count > 0 && lines[-1] == "") {
    lines.removeAt(-1)
}

for (i in 0...lines.count) {
    if (lines[i].endsWith("\r")) {
        lines[i] = lines[i][0...-1]
    }
}

var lineCnt = lines.count
var maxW = 0
for (line in lines) {
    if (line.count > maxW) maxW = line.count
}

var isSep = List.filled(maxW, true)
for (col in 0...maxW) {
    for (row in 0...lineCnt) {
        if (col < lines[row].count) {
            var char = lines[row][col]
            if (char != " " && char != "\t") {
                isSep[col] = false
                break
            }
        }
    }
}

var grandTotal = "0"

var processBlock = Fn.new { |start, end|
    var op = "+"
    var nums = []
    for (col in start..end) {
        var buf = ""
        for (row in 0...lineCnt) {
            if (col < lines[row].count) {
                var char = lines[row][col]
                var code = char.bytes[0]
                if (code >= 48 && code <= 57) {
                    buf = buf + char
                } else if (char == "+" || char == "*") {
                    op = char
                }
            }
        }
        if (buf != "") {
            nums.add(buf)
        }
    }
    if (nums.isEmpty) return
    var blockRes = ""
    if (op == "*") {
        blockRes = "1"
        for (num in nums) {
            blockRes = mulStr.call(blockRes, num)
        }
    } else {
        blockRes = "0"
        for (num in nums) {
            blockRes = addStr.call(blockRes, num)
        }
    }
    grandTotal = addStr.call(grandTotal, blockRes)
}

var inBlock = false
var start = 0
for (col in 0...maxW) {
    if (!isSep[col]) {
        if (!inBlock) {
            inBlock = true
            start = col
        }
    } else {
        if (inBlock) {
            processBlock.call(start, col - 1)
            inBlock = false
        }
    }
}
if (inBlock) {
    processBlock.call(start, maxW - 1)
}

System.print("Grand total: %(grandTotal)")
