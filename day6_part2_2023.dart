import 'dart:io';

int calculateWaysToWinLongRace(int time, int record) {
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

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();
  int time = 0;
  int distance = 0;

  for (String line in lines) {
    if (line.isNotEmpty) {
      List<String> parts = line.split(':');
      String value = parts[1].replaceAll(' ', '');
      if (time == 0) {
        time = int.parse(value);
      } else {
        distance = int.parse(value);
      }
    }
  }

  int waysToWin = calculateWaysToWinLongRace(time, distance);

  print(waysToWin);
}