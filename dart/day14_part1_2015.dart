
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  List<int> speeds = [];
  List<int> flyTimes = [];
  List<int> restTimes = [];
  
  for (String line in lines) {
    List<String> parts = line.split(' ');
    speeds.add(int.parse(parts[3]));
    flyTimes.add(int.parse(parts[6]));
    restTimes.add(int.parse(parts[13]));
  }
  
  int maxDistance = 0;
  
  for (int i = 0; i < speeds.length; i++) {
    int distance = calculateDistance(speeds[i], flyTimes[i], restTimes[i], 2503);
    if (distance > maxDistance) {
      maxDistance = distance;
    }
  }
  
  print(maxDistance);
}

int calculateDistance(int speed, int flyTime, int restTime, int totalTime) {
  int cycleTime = flyTime + restTime;
  int fullCycles = totalTime ~/ cycleTime;
  int remainingTime = totalTime % cycleTime;
  
  int distance = fullCycles * flyTime * speed;
  distance += (remainingTime > flyTime) ? flyTime * speed : remainingTime * speed;
  
  return distance;
}
