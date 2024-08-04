fs = require 'fs'

class Record
  constructor: (@timestamp, @action, @guardID) ->

readAndParseInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8')
  lines = data.split('\n')
  records = []
  layout = 'YYYY-MM-DD HH:mm'
  for line in lines
    parts = line.split('] ')
    timePart = parts[0].slice(1)
    actionPart = parts[1]
    ts = new Date(timePart.replace(' ', 'T'))
    guardID = -1
    if actionPart.includes 'Guard'
      match = actionPart.match /Guard #(\d+) begins shift/
      guardID = parseInt match[1]
      actionPart = 'begins shift'
    else if actionPart.includes 'falls asleep'
      actionPart = 'falls asleep'
    else if actionPart.includes 'wakes up'
      actionPart = 'wakes up'
    records.push new Record ts, actionPart, guardID
  records

records = readAndParseInput 'input.txt'
records.sort (a, b) -> a.timestamp - b.timestamp

guardSleepMinutes = {}
currentGuardID = null
sleepStart = null
for record in records
  switch record.action
    when 'begins shift'
      currentGuardID = record.guardID
    when 'falls asleep'
      sleepStart = record.timestamp
    when 'wakes up'
      if !guardSleepMinutes[currentGuardID]?
        guardSleepMinutes[currentGuardID] = new Array(60).fill(0)
      for i in [sleepStart.getMinutes()..record.timestamp.getMinutes()]
        guardSleepMinutes[currentGuardID][i]++

maxSleep = 0
sleepiestGuard = null
for guardID, minutes of guardSleepMinutes
  totalSleep = minutes.reduce (a, b) -> a + b
  if totalSleep > maxSleep
    maxSleep = totalSleep
    sleepiestGuard = guardID

maxMinute = 0
maxMinuteCount = 0
for i, count of guardSleepMinutes[sleepiestGuard]
  if count > maxMinuteCount
    maxMinuteCount = count
    maxMinute = i

console.log sleepiestGuard * maxMinute