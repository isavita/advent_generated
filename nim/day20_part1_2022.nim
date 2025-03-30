
import strutils, sequtils, os

type Num = object
  originalPos: int
  val: int64

proc floorMod(a, n: int64): int64 =
  var res = a mod n
  if res < 0:
    res += n
  return res

proc mix(nums: var seq[Num]) =
  let l = nums.len
  if l <= 1: return
  let n = int64(l - 1)

  var originalOrderIndices: seq[int] = @[]
  for i in 0 ..< l:
    originalOrderIndices.add(i)

  for originalIndex in originalOrderIndices:
    var currentIndex = -1
    for i in 0 ..< nums.len:
      if nums[i].originalPos == originalIndex:
        currentIndex = i
        break

    if currentIndex == -1:
        # Should not happen if input is consistent
        raise newException(ValueError, "Item not found during mix")

    let numToMove = nums[currentIndex]
    let val = numToMove.val

    if val == 0: continue # No movement needed

    nums.delete(currentIndex)

    # Calculate the target index based on movement val in a circle of size n
    let targetIndex = floorMod(currentIndex.int64 + val, n).int
    
    nums.insert(numToMove, targetIndex)


proc coords(nums: seq[Num]): int64 =
  let l = nums.len
  if l == 0: return 0

  var zeroIndex = -1
  for i, num in nums:
    if num.val == 0:
      zeroIndex = i
      break

  if zeroIndex == -1:
      # Assume 0 is always present based on problem context
      raise newException(ValueError, "Zero not found")

  var sumVal: int64 = 0
  sumVal += nums[(zeroIndex + 1000) mod l].val
  sumVal += nums[(zeroIndex + 2000) mod l].val
  sumVal += nums[(zeroIndex + 3000) mod l].val
  return sumVal

proc main =
  var nums: seq[Num] = @[]
  var i = 0
  for line in readFile("input.txt").strip.splitLines:
    if line.len > 0:
      let val = parseInt(line)
      nums.add(Num(originalPos: i, val: int64(val)))
      i += 1

  if nums.len == 0:
    echo 0
    return

  var mixedNums = nums # Value copy of seq structure, elements are objects
  mix(mixedNums)
  echo coords(mixedNums)

main()
