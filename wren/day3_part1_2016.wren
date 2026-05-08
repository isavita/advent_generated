import "io" for File

var isPossibleTriangle = Fn.new { |a, b, c| (a + b > c) && (a + c > b) && (b + c > a) }

var content = File.read("input.txt")
var count = 0
for (line in content.split("\n")) {
  var t = line.trim()
  if (t == "") continue
  var parts = t.split(" ")
  var nums = []
  for (s in parts) if (s != "") nums.add(Num.fromString(s))
  if (nums.count == 3 && isPossibleTriangle.call(nums[0], nums[1], nums[2])) count = count + 1
}
System.print(count)