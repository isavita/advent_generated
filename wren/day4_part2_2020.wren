
import "io" for File

var isDigits = Fn.new { |s, n|
    if (s == null || s.count != n) return false
    var bytes = s.bytes
    for (i in 0...n) {
        var c = bytes[i]
        if (c < 48 || c > 57) return false
    }
    return true
}

var valYear = Fn.new { |s, min, max|
    if (!isDigits.call(s, 4)) return false
    var y = Num.fromString(s)
    return y >= min && y <= max
}

var valHgt = Fn.new { |s|
    if (s == null) return false
    if (s.endsWith("cm")) {
        var v = s[0...s.count-2]
        if (v.count == 0 || !isDigits.call(v, v.count)) return false
        var n = Num.fromString(v)
        return n >= 150 && n <= 193
    } else if (s.endsWith("in")) {
        var v = s[0...s.count-2]
        if (v.count == 0 || !isDigits.call(v, v.count)) return false
        var n = Num.fromString(v)
        return n >= 59 && n <= 76
    }
    return false
}

var valHcl = Fn.new { |s|
    if (s == null || s.count != 7 || s[0] != "#") return false
    var bytes = s.bytes
    for (i in 1..6) {
        var c = bytes[i]
        var isHex = (c >= 48 && c <= 57) || (c >= 97 && c <= 102)
        if (!isHex) return false
    }
    return true
}

var ecls = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
var valEcl = Fn.new { |s| ecls.contains(s) }
var valPid = Fn.new { |s| isDigits.call(s, 9) }

var content = File.read("input.txt").replace("\r", "")
var records = content.split("\n\n")
var count = 0

for (rec in records) {
    if (rec.trim() == "") continue
    var fields = rec.replace("\n", " ").split(" ")
    var map = {}
    for (f in fields) {
        if (f != "") {
            var parts = f.split(":")
            if (parts.count >= 2) {
                map[parts[0]] = parts[1]
            }
        }
    }
    
    if (valYear.call(map["byr"], 1920, 2002) &&
        valYear.call(map["iyr"], 2010, 2020) &&
        valYear.call(map["eyr"], 2020, 2030) &&
        valHgt.call(map["hgt"]) &&
        valHcl.call(map["hcl"]) &&
        valEcl.call(map["ecl"]) &&
        valPid.call(map["pid"])) {
        count = count + 1
    }
}

System.print(count)
