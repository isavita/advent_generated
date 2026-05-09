
import "io" for File

class Record {
  construct new(year, month, day, hour, minute, event) {
    _year = year
    _month = month
    _day = day
    _hour = hour
    _minute = minute
    _event = event
  }

  year { _year }
  month { _month }
  day { _day }
  hour { _hour }
  minute { _minute }
  event { _event }

  // For sorting
  isBefore(other) {
    if (_year != other.year) return _year < other.year
    if (_month != other.month) return _month < other.month
    if (_day != other.day) return _day < other.day
    if (_hour != other.hour) return _hour < other.hour
    return _minute < other.minute
  }
}

class Guard {
  construct new(id) {
    _id = id
    _minutes = (0..59).map { 0 }.toList
  }

  id { _id }
  minutes { _minutes }

  addSleep(start, end) {
    for (i in start...end) {
      _minutes[i] = _minutes[i] + 1
    }
  }
}

var main = Fn.new {
  var content = File.read("input.txt").trim()
  var lines = content.split("\n").where { |line| line.trim() != "" }.toList

  var records = []

  for (line in lines) {
    var timeStr = line[1..16]
    var event = line[19..-1]

    var parts = timeStr.split(" ")
    var dateParts = parts[0].split("-")
    var timeParts = parts[1].split(":")

    var year = Num.fromString(dateParts[0])
    var month = Num.fromString(dateParts[1])
    var day = Num.fromString(dateParts[2])
    var hour = Num.fromString(timeParts[0])
    var minute = Num.fromString(timeParts[1])

    records.add(Record.new(year, month, day, hour, minute, event))
  }

  // Sort records by time
  for (i in 0...records.count - 1) {
    for (j in i + 1...records.count) {
      if (!records[i].isBefore(records[j])) {
        var temp = records[i]
        records[i] = records[j]
        records[j] = temp
      }
    }
  }

  var guards = {}
  var currentGuard = null
  var sleepStart = null

  for (record in records) {
    if (record.event.contains("begins shift")) {
      var parts = record.event.split(" ")
      var idStr = parts[1]
      var id = Num.fromString(idStr[1..-1])
      if (!guards.containsKey(id)) {
        guards[id] = Guard.new(id)
      }
      currentGuard = guards[id]
    } else if (record.event.contains("falls asleep")) {
      sleepStart = record.minute
    } else if (record.event.contains("wakes up")) {
      if (currentGuard != null && sleepStart != null) {
        currentGuard.addSleep(sleepStart, record.minute)
      }
    }
  }

  var maxCount = 0
  var maxGuardId = null
  var maxMinute = null

  for (guardId in guards.keys) {
    var guard = guards[guardId]
    for (i in 0..59) {
      if (guard.minutes[i] > maxCount) {
        maxCount = guard.minutes[i]
        maxGuardId = guardId
        maxMinute = i
      }
    }
  }

  System.print(maxGuardId * maxMinute)
}

main.call()
