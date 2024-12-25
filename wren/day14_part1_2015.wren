
import "io" for File

class Reindeer {
  construct new(speed, flyTime, restTime) {
    _speed = speed
    _flyTime = flyTime
    _restTime = restTime
    _distance = 0
    _flying = true
    _timeInMode = 0
  }

  simulate() {
    if (_flying) {
      _distance = _distance + _speed
      _timeInMode = _timeInMode + 1
      if (_timeInMode == _flyTime) {
        _flying = false
        _timeInMode = 0
      }
    } else {
      _timeInMode = _timeInMode + 1
      if (_timeInMode == _restTime) {
        _flying = true
        _timeInMode = 0
      }
    }
  }

  distance { _distance }
}

var reindeers = []
var lines = File.read("input.txt").split("\n")
for (line in lines) {
  var parts = line.split(" ")
  if (parts.count > 1) {
    var speed = Num.fromString(parts[3])
    var flyTime = Num.fromString(parts[6])
    var restTime = Num.fromString(parts[13])
    reindeers.add(Reindeer.new(speed, flyTime, restTime))
  }
}

var totalSeconds = 2503
for (i in 0...totalSeconds) {
  for (reindeer in reindeers) {
    reindeer.simulate()
  }
}

var maxDistance = 0
for (reindeer in reindeers) {
  if (reindeer.distance > maxDistance) {
    maxDistance = reindeer.distance
  }
}

System.print(maxDistance)
