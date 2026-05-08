import "io" for File

var isValidTriangle = Fn.new { |a,b,c|
    return (a+b>c) && (a+c>b) && (b+c>a)
}

var content = File.read("input.txt")
var lines = content.split("\n")
var rows = []
for (line in lines) {
    var trimmed = line.trim()
    if (trimmed != "") {
        var parts = trimmed.split(" ")
        var nums = []
        for (p in parts) {
            if (p != "") {
                nums.add(Num.fromString(p))
            }
        }
        if (nums.count >= 3) {
            rows.add(nums)
        }
    }
}

var validCount = 0
var numRows = rows.count
for (col in 0..2) {
    var i = 0
    while (i+2 < numRows) {
        var a = rows[i][col]
        var b = rows[i+1][col]
        var c = rows[i+2][col]
        if (isValidTriangle.call(a,b,c)) {
            validCount = validCount + 1
        }
        i = i + 3
    }
}

System.print(validCount)