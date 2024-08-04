fs = require 'fs'

class Record
  constructor: (@time, @event) ->

class Guard
  constructor: (@id) ->
    @minutes = Array(60).fill(0)
    @totalMin = 0

input = fs.readFileSync('input.txt', 'utf8').split('\n')
records = []
guards = {}

for line in input
  time = new Date(line.slice(1, 17))
  event = line.slice(19)
  records.push new Record(time, event)

records.sort (a, b) -> a.time - b.time

currentGuard = null
sleepStart = 0

for record in records
  switch
    when record.event.includes "begins shift"
      id = parseInt(record.event.split(' ')[1].slice(1))
      guards[id] ?= new Guard(id)
      currentGuard = guards[id]
    when record.event.includes "falls asleep"
      sleepStart = record.time.getMinutes()
    when record.event.includes "wakes up"
      for i in [sleepStart...record.time.getMinutes()]
        currentGuard.minutes[i]++
        currentGuard.totalMin++

mostFreqGuard = null
mostFreqMin = 0

for id, guard of guards
  for i in [0...60]
    if !mostFreqGuard? or guard.minutes[i] > mostFreqGuard.minutes[mostFreqMin]
      mostFreqGuard = guard
      mostFreqMin = i

console.log mostFreqGuard.id * mostFreqMin