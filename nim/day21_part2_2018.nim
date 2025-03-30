
import std/sets

const
  mask24 = 16777215 # (1 shl 24) - 1
  magic1 = 65536   # 1 shl 16
  magic2 = 7586220
  mask8  = 255      # (1 shl 8) - 1
  magic3 = 65899
  shiftVal = 256    # 1 shl 8

proc main() =
  var register5 = 0
  var seen = initHashSet[int]()
  var last_unique = 0

  while true:
    var register3 = register5 or magic1
    register5 = magic2

    while true:
      let register1 = register3 and mask8
      register5 = (((register5 + register1) and mask24) * magic3) and mask24

      if register3 < shiftVal:
        if seen.contains(register5):
          echo last_unique
          return
        seen.incl(register5)
        last_unique = register5
        break
      else:
        register3 = register3 div shiftVal

when isMainModule:
  main()
