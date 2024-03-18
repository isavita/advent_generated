import 'dart:io';
import 'dart:math';
import 'dart:convert';
import 'dart:core';

class Nanobot {
  int x, y, z, radius;
  Nanobot(this.x, this.y, this.z, this.radius);
}

void main() {
  final file = File('input.txt');
  final nanobots = parseNanobots(file.readAsLinesSync());
  final strongest = findStrongestNanobot(nanobots);
  final inRangeCount = countNanobotsInRange(nanobots, strongest);
  print(inRangeCount);
}

List<Nanobot> parseNanobots(List<String> lines) {
  final nanobots = <Nanobot>[];
  final regex = RegExp(r'pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)');
  for (final line in lines) {
    final match = regex.firstMatch(line);
    if (match != null) {
      final x = int.parse(match.group(1)!);
      final y = int.parse(match.group(2)!);
      final z = int.parse(match.group(3)!);
      final radius = int.parse(match.group(4)!);
      nanobots.add(Nanobot(x, y, z, radius));
    }
  }
  return nanobots;
}

Nanobot findStrongestNanobot(List<Nanobot> nanobots) {
  Nanobot strongest = nanobots.first;
  for (final nanobot in nanobots) {
    if (nanobot.radius > strongest.radius) {
      strongest = nanobot;
    }
  }
  return strongest;
}

int countNanobotsInRange(List<Nanobot> nanobots, Nanobot strongest) {
  int count = 0;
  for (final nanobot in nanobots) {
    if (manhattanDistance(nanobot, strongest) <= strongest.radius) {
      count++;
    }
  }
  return count;
}

int manhattanDistance(Nanobot a, Nanobot b) {
  return (a.x - b.x).abs() + (a.y - b.y).abs() + (a.z - b.z).abs();
}