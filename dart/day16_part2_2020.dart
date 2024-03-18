import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:math';

class Rule {
  final String name;
  final List<List<int>> ranges;

  Rule(this.name, this.ranges);

  bool isValid(int value) {
    for (final range in ranges) {
      if (value >= range[0] && value <= range[1]) {
        return true;
      }
    }
    return false;
  }
}

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  final rules = <Rule>[];
  final myTicket = <int>[];
  final nearbyTickets = <List<int>>[];
  int section = 0;

  for (final line in lines) {
    if (line.isEmpty) {
      section++;
      continue;
    }

    switch (section) {
      case 0:
        final parts = RegExp(r'^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$').firstMatch(line);
        if (parts != null) {
          rules.add(Rule(
            parts[1]!,
            [
              [int.parse(parts[2]!), int.parse(parts[3]!)],
              [int.parse(parts[4]!), int.parse(parts[5]!)],
            ],
          ));
        }
        break;
      case 1:
        if (line != 'your ticket:') {
          myTicket.addAll(parseTicket(line));
        }
        break;
      case 2:
        if (line != 'nearby tickets:') {
          final ticket = parseTicket(line);
          if (isValidTicket(ticket, rules)) {
            nearbyTickets.add(ticket);
          }
        }
        break;
    }
  }

  final fieldPositions = solveFieldPositions(rules, nearbyTickets);
  final departureProduct = calculateDepartureProduct(myTicket, fieldPositions);

  print(departureProduct);
}

List<int> parseTicket(String s) {
  return s.split(',').map(int.parse).toList();
}

bool isValidTicket(List<int> ticket, List<Rule> rules) {
  for (final value in ticket) {
    if (!isValidForAnyRule(value, rules)) {
      return false;
    }
  }
  return true;
}

bool isValidForAnyRule(int value, List<Rule> rules) {
  for (final rule in rules) {
    if (rule.isValid(value)) {
      return true;
    }
  }
  return false;
}

Map<String, int> solveFieldPositions(List<Rule> rules, List<List<int>> tickets) {
  final validPositions = <String, Set<int>>{};
  for (final rule in rules) {
    validPositions[rule.name] = <int>{};
    for (var i = 0; i < tickets[0].length; i++) {
      var valid = true;
      for (final ticket in tickets) {
        if (!rule.isValid(ticket[i])) {
          valid = false;
          break;
        }
      }
      if (valid) {
        validPositions[rule.name]!.add(i);
      }
    }
  }

  final fieldPositions = <String, int>{};
  while (fieldPositions.length < rules.length) {
    final singlePositions = validPositions.entries.where((e) => e.value.length == 1).toList();
    for (final entry in singlePositions) {
      final pos = entry.value.first;
      fieldPositions[entry.key] = pos;
      for (final otherName in validPositions.keys) {
        if (otherName != entry.key) {
          validPositions[otherName]?.remove(pos);
        }
      }
    }
    validPositions.removeWhere((key, value) => value.isEmpty);
  }
  return fieldPositions;
}

int calculateDepartureProduct(List<int> ticket, Map<String, int> fieldPositions) {
  int product = 1;
  for (final entry in fieldPositions.entries) {
    if (entry.key.startsWith('departure')) {
      product *= ticket[entry.value];
    }
  }
  return product;
}