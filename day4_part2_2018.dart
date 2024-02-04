import 'dart:io';

class Record {
  DateTime time;
  String event;

  Record(this.time, this.event);
}

class Guard {
  int id;
  List<int> minutes = List.filled(60, 0);
  int totalMin = 0;

  Guard(this.id);
}

void main() {
  var file = new File('input.txt');
  var lines = file.readAsLinesSync();

  List<Record> records = [];
  Map<int, Guard> guards = {};

  for (var line in lines) {
    var t = DateTime.parse(line.substring(1, 17));
    records.add(Record(t, line.substring(19)));
  }

  records.sort((a, b) => a.time.compareTo(b.time));

  Guard? currentGuard;
  int sleepStart = 0;

  for (var record in records) {
    if (record.event.contains('begins shift')) {
      var id = int.parse(record.event.split(' ')[1].substring(1));
      if (!guards.containsKey(id)) {
        guards[id] = Guard(id);
      }
      currentGuard = guards[id];
    } else if (record.event.contains('falls asleep')) {
      sleepStart = record.time.minute;
    } else if (record.event.contains('wakes up')) {
      for (var i = sleepStart; i < record.time.minute; i++) {
        currentGuard!.minutes[i]++;
        currentGuard.totalMin++;
      }
    }
  }

  Guard? mostFreqGuard;
  int mostFreqMin = 0;

  guards.forEach((key, value) {
    value.minutes.asMap().forEach((i, m) {
      if (mostFreqGuard == null || m > mostFreqGuard!.minutes[mostFreqMin]) {
        mostFreqGuard = value;
        mostFreqMin = i;
      }
    });
  });

  print(mostFreqGuard!.id * mostFreqMin);
}