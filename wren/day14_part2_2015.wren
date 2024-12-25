
import "io" for File

class Reindeer {
  construct new(speed, flyTime, restTime) {
    _speed = speed
    _flyTime = flyTime
    _restTime = restTime
    _distance = 0
    _points = 0
    _flying = true
    _timeInMode = 0
  }

  speed { _speed }
  distance { _distance }
  points { _points }

  advance() {
    if (_flying) {
      _distance = _distance + _speed
    }
    _timeInMode = _timeInMode + 1
    if (_flying && _timeInMode == _flyTime) {
      _flying = false
      _timeInMode = 0
    } else if (!_flying && _timeInMode == _restTime) {
      _flying = true
      _timeInMode = 0
    }
  }

  awardPoint() {
    _points = _points + 1
  }
}

var readReindeerDetails = Fn.new { |filename|
  var reindeers = []
  var lines = File.read(filename).split("\n")
  for (line in lines) {
    if (line.trim() == "") continue
    var parts = line.split(" ")
    var speed = Num.fromString(parts[3])
    var flyTime = Num.fromString(parts[6])
    var restTime = Num.fromString(parts[13])
    reindeers.add(Reindeer.new(speed, flyTime, restTime))
  }
  return reindeers
}

var simulateRaceWithPoints = Fn.new { |reindeers, totalSeconds|
  for (i in 0...totalSeconds) {
    var maxDistance = 0
    for (reindeer in reindeers) {
      reindeer.advance()
      if (reindeer.distance > maxDistance) {
        maxDistance = reindeer.distance
      }
    }
    for (reindeer in reindeers) {
      if (reindeer.distance == maxDistance) {
        reindeer.awardPoint()
      }
    }
  }
}

var findMaxPoints = Fn.new { |reindeers|
  var maxPoints = 0
  for (reindeer in reindeers) {
    if (reindeer.points > maxPoints) {
      maxPoints = reindeer.points
    }
  }
  return maxPoints
}

var reindeers = readReindeerDetails.call("input.txt")
simulateRaceWithPoints.call(reindeers, 2503)
System.print(findMaxPoints.call(reindeers))
