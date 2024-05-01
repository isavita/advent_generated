import strutils, sequtils, math

type Num = object
  pos, val: int

proc readAll(path: string): string =
  readFile(path).strip

proc toInt(s: string): int =
  parseInt(s)

proc mix(nums: var seq[Num]) =
  let n = nums.len - 1
  for i in 0..<nums.len:
    let oldpos = nums[i].pos
    let newpos = ((oldpos + nums[i].val) mod n + n) mod n
    if oldpos < newpos:
      for j in 0..<nums.len:
        if nums[j].pos > oldpos and nums[j].pos <= newpos:
          nums[j].pos.dec
    elif newpos < oldpos:
      for j in 0..<nums.len:
        if nums[j].pos >= newpos and nums[j].pos < oldpos:
          nums[j].pos.inc
    nums[i].pos = newpos

proc coords(nums: seq[Num]): int =
  let l = nums.len
  var zeroPos: int
  for i in 0..<nums.len:
    if nums[i].val == 0:
      zeroPos = nums[i].pos
      break
  var sum: int
  for i in 0..<nums.len:
    if nums[i].pos == (zeroPos + 1000) mod l or nums[i].pos == (zeroPos + 2000) mod l or nums[i].pos == (zeroPos + 3000) mod l:
      sum += nums[i].val
  sum

when isMainModule:
  let nums = readAll("input.txt").splitLines.mapIt(Num(pos: parseInt($it), val: parseInt($it)))
  var nums2 = newSeq[Num](nums.len)
  for i in 0..<nums.len:
    nums2[i] = Num(pos: i, val: nums[i].val * 811589153)
  for _ in 0..<10:
    mix(nums2)
  echo coords(nums2)