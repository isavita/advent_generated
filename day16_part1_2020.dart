import 'dart:io';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();

  List<String> rules = [];
  List<int> myTicket = [];
  List<List<int>> nearbyTickets = [];

  int section = 0;

  for (String line in input) {
    if (line.isEmpty) {
      section++;
      continue;
    }

    if (section == 0) {
      rules.add(line);
    } else if (section == 1) {
      if (!line.contains(',')) continue;
      myTicket = line.split(',').map(int.parse).toList();
    } else {
      if (!line.contains(',')) continue;
      nearbyTickets.add(line.split(',').map(int.parse).toList());
    }
  }

  List<List<int>> validRanges = [];
  for (String rule in rules) {
    List<String> parts = rule.split(': ')[1].split(' or ');
    for (String part in parts) {
      List<int> range = part.split('-').map(int.parse).toList();
      validRanges.add([range[0], range[1]]);
    }
  }

  int errorRate = 0;
  for (List<int> ticket in nearbyTickets) {
    for (int value in ticket) {
      bool valid = false;
      for (List<int> range in validRanges) {
        if (value >= range[0] && value <= range[1]) {
          valid = true;
          break;
        }
      }
      if (!valid) {
        errorRate += value;
      }
    }
  }

  print(errorRate);
}