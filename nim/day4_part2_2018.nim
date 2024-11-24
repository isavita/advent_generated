
import std/[algorithm, strutils, times, parseutils, tables, strformat]

type
  Record = object
    time: DateTime
    event: string

  Guard = object
    id: int
    minutes: array[60, int]
    totalMin: int

proc main() =
  var records: seq[Record]
  var guards = initTable[int, Guard]()

  block readInput:
    let content = readFile("input.txt").strip().splitLines()
    for line in content:
      let 
        timeStr = line[1..16]
        event = line[19..^1]
        time = parse(timeStr, "yyyy-MM-dd HH:mm")
      records.add(Record(time: time, event: event))

  records.sort do (a, b: Record) -> int:
    cmp(a.time, b.time)

  var 
    currentGuard: ptr Guard
    sleepStart: int

  for record in records:
    if "begins shift" in record.event:
      let id = record.event.split()[1][1..^1].parseInt()
      if id notin guards:
        guards[id] = Guard(id: id)
      currentGuard = guards[id].addr

    elif "falls asleep" in record.event:
      sleepStart = record.time.minute

    elif "wakes up" in record.event:
      for i in sleepStart..<record.time.minute:
        inc currentGuard.minutes[i]
        inc currentGuard.totalMin

  var 
    mostFreqGuard: ptr Guard
    mostFreqMin: int

  for g in guards.mvalues:
    for i, m in g.minutes:
      if mostFreqGuard.isNil or m > mostFreqGuard.minutes[mostFreqMin]:
        mostFreqGuard = g.addr
        mostFreqMin = i

  echo mostFreqGuard.id * mostFreqMin

main()
