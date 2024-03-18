import 'dart:io';

int calculateWaysToWin(int time, int record) {
  int waysToWin = 0;
  for (int holdTime = 1; holdTime < time; holdTime++) {
    int travelTime = time - holdTime;
    int distance = holdTime * travelTime;
    if (distance > record) {
      waysToWin++;
    }
  }
  return waysToWin;
}

void main() async {
  try {
    final file = File('input.txt');
    final lines = await file.readAsLines();

    final times = lines[0].split(' ').map(int.tryParse).whereType<int>().toList();
    final distances = lines[1].split(' ').map(int.tryParse).whereType<int>().toList();

    if (times.length != distances.length) {
      print('Error: The number of times and distances must be equal.');
      return;
    }

    int totalWays = 1;
    for (int i = 0; i < times.length; i++) {
      int ways = calculateWaysToWin(times[i], distances[i]);
      totalWays *= ways;
    }

    print(totalWays);
  } catch (e) {
    print('Error: $e');
  }
}