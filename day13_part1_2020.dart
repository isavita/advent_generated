
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int earliestTimestamp = int.parse(lines[0]);
  List<String> busIDs = lines[1].split(',');

  int minWaitTime = int.parse(busIDs[0]);
  int minBusID = int.parse(busIDs[0]);

  for (int i = 1; i < busIDs.length; i++) {
    if (busIDs[i] == 'x') continue;
    int busID = int.parse(busIDs[i]);
    int waitTime = busID - (earliestTimestamp % busID);
    if (waitTime < minWaitTime) {
      minWaitTime = waitTime;
      minBusID = busID;
    }
  }

  print(minBusID * minWaitTime);
}
