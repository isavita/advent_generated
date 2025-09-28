import os, strutils

type
  LogEntry = object
    timestamp: string
    action: string

const
  MAXG = 10000

type
  MinCounts = array[0..59, int]

var
  totalSleep: array[0..MAXG-1, int]
  minuteCounts: array[0..MAXG-1, MinCounts]

proc parseInt(s: string): int =
  var i = 0
  var sign = 1
  if i < s.len and s[i] == '-':
    sign = -1
    inc(i)
  var v = 0
  while i < s.len:
    let c = s[i]
    if c >= '0' and c <= '9':
      v = v * 10 + (int(c) - int('0'))
    else:
      break
    inc(i)
  result = sign * v

proc main() =
  let content = readFile("input.txt")
  let lines = splitLines(content)
  var records: seq[LogEntry] = @[]
  for line in lines:
    # assume proper format
    let timestamp = line[1 .. 16]
    let action = line[19 .. line.len - 1]
    records.add(LogEntry(timestamp: timestamp, action: action))

  # simple selection sort by timestamp
  for i in 0 ..< records.len - 1:
    var minIdx = i
    for j in i + 1 ..< records.len:
      if records[j].timestamp < records[minIdx].timestamp:
        minIdx = j
    if minIdx != i:
      let tmp = records[i]
      records[i] = records[minIdx]
      records[minIdx] = tmp

  var currentGuard = -1
  var asleepMinute = -1

  for rec in records:
    if rec.action.find("Guard") != -1:
      let hashPos = rec.action.find('#')
      let spacePos = rec.action.find(' ', hashPos)
      if hashPos >= 0 and spacePos > hashPos:
        let idStr = rec.action[(hashPos + 1) .. (spacePos - 1)]
        let gid = parseInt(idStr)
        currentGuard = gid
    elif rec.action.find("falls asleep") != -1:
      asleepMinute = parseInt(rec.timestamp[14 .. 15])
    elif rec.action.find("wakes up") != -1:
      let wakeUpMinute = parseInt(rec.timestamp[14 .. 15])
      if currentGuard >= 0 and asleepMinute >= 0:
        for m in asleepMinute ..< wakeUpMinute:
          totalSleep[currentGuard] += 1
          minuteCounts[currentGuard][m] += 1

  var mostGuard = -1
  var maxTotal = -1
  for g in 0 ..< MAXG:
    if totalSleep[g] > maxTotal:
      maxTotal = totalSleep[g]
      mostGuard = g

  var mostMinute = -1
  var maxMinuteCount = -1
  if mostGuard >= 0:
    for m in 0 ..< 60:
      if minuteCounts[mostGuard][m] > maxMinuteCount:
        maxMinuteCount = minuteCounts[mostGuard][m]
        mostMinute = m

  if mostGuard >= 0 and mostMinute >= 0:
    echo (mostGuard * mostMinute)
  else:
    echo 0

when isMainModule:
  main()