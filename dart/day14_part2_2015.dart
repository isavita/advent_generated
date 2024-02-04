
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
  List<int> distances = List.filled(speeds.length, 0);
  List<int> points = List.filled(speeds.length, 0);
  
  for (int i = 1; i <= 2503; i++) {
    for (int j = 0; j < speeds.length; j++) {
      if (i % (flyTimes[j] + restTimes[j]) <= flyTimes[j] && i % (flyTimes[j] + restTimes[j]) > 0) {
        distances[j] += speeds[j];
      }
    }
    
    maxDistance = distances.reduce((curr, next) => curr > next ? curr : next);
    
    for (int j = 0; j < speeds.length; j++) {
      if (distances[j] == maxDistance) {
        points[j]++;
      }
    }
  }
  
  print(points.reduce((curr, next) => curr > next ? curr : next));
}
