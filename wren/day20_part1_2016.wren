
import "io" for File
import "os" for Process

class IpRange {
  construct new(start, end) {
    _start = start
    _end = end
  }
  start { _start }
  end { _end }
}

var readIpRanges = Fn.new { |filename|
  var file = File.open(filename)
  var ranges = []
  var lines = file.readBytes(file.size).split("\n")
  for (line in lines) {
    if (line.count > 0) {
      var parts = line.split("-")
      var start = Num.fromString(parts[0])
      var end = Num.fromString(parts[1])
      ranges.add(IpRange.new(start, end))
    }
  }
  file.close()
  return ranges
}

var findUnblockedIp = Fn.new { |ranges|
  var currentIp = 0
  for (r in ranges) {
    if (r.start > currentIp) {
      return currentIp
    }
    if (r.end >= currentIp) {
      currentIp = r.end + 1
    }
  }
  return currentIp
}

var ipRanges = readIpRanges.call("input.txt")
ipRanges.sort {|a, b| a.start < b.start }
var unblockedIp = findUnblockedIp.call(ipRanges)
System.print(unblockedIp)
