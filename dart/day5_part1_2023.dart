import 'dart:io';

class RangeMap {
  int srcStart;
  int destStart;
  int length;

  RangeMap(this.srcStart, this.destStart, this.length);
}

int convertNumber(int number, List<RangeMap> ranges) {
  for (var r in ranges) {
    if (number >= r.srcStart && number < r.srcStart + r.length) {
      return r.destStart + (number - r.srcStart);
    }
  }
  return number;
}

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  List<int> seeds = [];
  List<RangeMap> currentRanges = [];
  List<List<RangeMap>> maps = [];

  for (var line in lines) {
    if (line.contains('map:')) {
      if (currentRanges.isNotEmpty) {
        maps.add(List.from(currentRanges));
        currentRanges = [];
      }
    } else if (line.startsWith('seeds:')) {
      var seedStrs = line.substring(7).split(' ');
      for (var s in seedStrs) {
        seeds.add(int.parse(s));
      }
    } else {
      var numbers = line.split(' ');
      if (numbers.length == 3) {
        var srcStart = int.parse(numbers[1]);
        var destStart = int.parse(numbers[0]);
        var length = int.parse(numbers[2]);

        currentRanges.add(RangeMap(srcStart, destStart, length));
      }
    }
  }
  maps.add(List.from(currentRanges));

  var minLocation = -1;
  for (var seed in seeds) {
    var location = seed;
    for (var m in maps) {
      location = convertNumber(location, m);
    }

    if (minLocation == -1 || location < minLocation) {
      minLocation = location;
    }
  }

  print(minLocation);
}