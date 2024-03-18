import 'dart:io';
import 'dart:convert';
import 'dart:math';

class Record {
  DateTime timestamp;
  String action;
  int guardID;

  Record(this.timestamp, this.action, this.guardID);
}

void main() {
  List<Record> records = readAndParseInput('input.txt');
  records.sort((a, b) => a.timestamp.compareTo(b.timestamp));

  Map<int, List<int>> guardSleepMinutes = {};
  int currentGuardID = -1;
  DateTime sleepStart = DateTime.now();

  for (var record in records) {
    switch (record.action) {
      case 'begins shift':
        currentGuardID = record.guardID;
        break;
      case 'falls asleep':
        sleepStart = record.timestamp;
        break;
      case 'wakes up':
        if (!guardSleepMinutes.containsKey(currentGuardID)) {
          guardSleepMinutes[currentGuardID] = List.filled(60, 0);
        }
        for (int i = sleepStart.minute; i < record.timestamp.minute; i++) {
          guardSleepMinutes[currentGuardID]![i]++;
        }
        break;
    }
  }

  int maxSleep = 0;
  int sleepiestGuard = -1;
  for (var guardID in guardSleepMinutes.keys) {
    int totalSleep = guardSleepMinutes[guardID]!.reduce((a, b) => a + b);
    if (totalSleep > maxSleep) {
      maxSleep = totalSleep;
      sleepiestGuard = guardID;
    }
  }

  int maxMinute = 0;
  int maxMinuteCount = 0;
  for (int i = 0; i < 60; i++) {
    if (guardSleepMinutes[sleepiestGuard]![i] > maxMinuteCount) {
      maxMinuteCount = guardSleepMinutes[sleepiestGuard]![i];
      maxMinute = i;
    }
  }

  print(sleepiestGuard * maxMinute);
}

List<Record> readAndParseInput(String filename) {
  List<Record> records = [];
  File file = File(filename);
  for (String line in file.readAsLinesSync()) {
    List<String> parts = line.split('] ');
    DateTime timestamp = DateTime.parse(parts[0].substring(1));
    String action = parts[1];
    int guardID = -1;
    if (action.startsWith('Guard')) {
      guardID = int.parse(action.split(' ')[1].substring(1));
      action = 'begins shift';
    } else if (action.startsWith('falls')) {
      action = 'falls asleep';
    } else if (action.startsWith('wakes')) {
      action = 'wakes up';
    }
    records.add(Record(timestamp, action, guardID));
  }
  return records;
}