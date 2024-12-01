
import 'dart:io';
import 'dart:convert';

Future<void> main() async {
  final file = File('input.txt');
  final contents = await file.readAsString();
  final lines = contents.split('\n');

  List<int> leftList = [];
  List<int> rightList = [];

  for (final line in lines) {
    final parts = line.trim().split('   ');
    if (parts.length == 2) {
      leftList.add(int.parse(parts[0]));
      rightList.add(int.parse(parts[1]));
    }
  }

  leftList.sort();
  rightList.sort();

  int totalDistance = 0;
  for (int i = 0; i < leftList.length; i++) {
    totalDistance += (leftList[i] - rightList[i]).abs();
  }

  print('Total distance: $totalDistance');
}
