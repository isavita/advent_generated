
import 'dart:io';

void main() {
  var file = File('input.txt');
  var lanternFishCounts = List<int>.filled(9, 0);

  file.readAsLines().then((lines) {
    for (var line in lines) {
      var fishAges = line.split(',');
      for (var age in fishAges) {
        var ageCount = int.parse(age);
        lanternFishCounts[ageCount]++;
      }
    }

    for (var i = 0; i < 256; i++) {
      var newLanternFish = lanternFishCounts[0];
      for (var j = 0; j < lanternFishCounts.length - 1; j++) {
        lanternFishCounts[j] = lanternFishCounts[j + 1];
      }
      lanternFishCounts[6] += newLanternFish;
      lanternFishCounts[8] = newLanternFish;
    }

    print(sum(lanternFishCounts));
  });
}

int sum(List<int> numbers) {
  var sum = 0;
  for (var num in numbers) {
    sum += num;
  }
  return sum;
}
