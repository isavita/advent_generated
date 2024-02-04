import 'dart:io';

class RangeMap {
  int srcStart, destStart, length;

  RangeMap(this.srcStart, this.destStart, this.length);
}

int reverseConvertNumber(int number, List<RangeMap> ranges) {
  for (int i = ranges.length - 1; i >= 0; i--) {
    RangeMap r = ranges[i];
    if (number >= r.destStart && number < r.destStart + r.length) {
      return r.srcStart + (number - r.destStart);
    }
  }
  return number;
}

bool isInSeedRanges(int number, List<List<int>> ranges) {
  for (List<int> r in ranges) {
    if (number >= r[0] && number < r[0] + r[1]) {
      return true;
    }
  }
  return false;
}

void main() {
  File file = File('input.txt');
  List<List<int>> seedRanges = [];
  List<RangeMap> currentRanges = [];
  List<List<RangeMap>> maps = [];

  file.readAsLines().then((List<String> lines) {
    for (String line in lines) {
      if (line.contains('map:')) {
        if (currentRanges.length > 0) {
          maps.add(List.from(currentRanges));
          currentRanges = [];
        }
      } else if (line.startsWith('seeds:')) {
        List<String> seedStrs = line.substring(7).split(' ');
        for (int i = 0; i < seedStrs.length; i += 2) {
          int start = int.parse(seedStrs[i]);
          int length = int.parse(seedStrs[i + 1]);
          seedRanges.add([start, length]);
        }
      } else {
        List<String> numbers = line.split(' ');
        if (numbers.length == 3) {
          int srcStart = int.parse(numbers[1]);
          int destStart = int.parse(numbers[0]);
          int length = int.parse(numbers[2]);

          currentRanges.add(RangeMap(srcStart, destStart, length));
        }
      }
    }
    if (currentRanges.length > 0) {
      maps.add(List.from(currentRanges));
    }

    int location = 0;
    while (true) {
      int seed = location;
      for (int i = maps.length - 1; i >= 0; i--) {
        seed = reverseConvertNumber(seed, maps[i]);
      }

      if (isInSeedRanges(seed, seedRanges)) {
        print(location);
        break;
      }
      location++;
    }
  });
}