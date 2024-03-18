import 'dart:io';
import 'dart:collection';

void main() {
  final happinessMap = readHappinessValues('input.txt');
  addYourself(happinessMap);
  final guests = getGuestList(happinessMap);
  final maxHappiness = calculateOptimalArrangement(guests, happinessMap);
  print(maxHappiness);
}

Map<String, Map<String, int>> readHappinessValues(String filename) {
  final file = File(filename).readAsLinesSync();
  final happinessMap = <String, Map<String, int>>{};
  for (final line in file) {
    final parts = line.split(' ');
    if (parts.length < 11) continue;
    final from = parts[0];
    final to = parts[10].substring(0, parts[10].length - 1);
    final change = int.parse(parts[3]) * (parts[2] == 'lose' ? -1 : 1);
    if (!happinessMap.containsKey(from)) {
      happinessMap[from] = <String, int>{};
    }
    happinessMap[from]![to] = change;
  }
  return happinessMap;
}

void addYourself(Map<String, Map<String, int>> happinessMap) {
  happinessMap['You'] = <String, int>{};
  for (final guest in happinessMap.keys) {
    happinessMap[guest]!['You'] = 0;
    happinessMap['You']![guest] = 0;
  }
}

List<String> getGuestList(Map<String, Map<String, int>> happinessMap) {
  return happinessMap.keys.toList();
}

int calculateOptimalArrangement(List<String> guests, Map<String, Map<String, int>> happinessMap) {
  int maxHappiness = 0;
  permute(guests, 0, (happiness) {
    if (happiness > maxHappiness) {
      maxHappiness = happiness;
    }
  }, happinessMap);
  return maxHappiness;
}

void permute(List<String> arr, int i, void Function(int) callback, Map<String, Map<String, int>> happinessMap) {
  if (i > arr.length) return;
  if (i == arr.length) {
    callback(calculateHappiness(arr, happinessMap));
    return;
  }
  for (int j = i; j < arr.length; j++) {
    _swap(arr, i, j);
    permute(arr, i + 1, callback, happinessMap);
    _swap(arr, i, j);
  }
}

int calculateHappiness(List<String> arrangement, Map<String, Map<String, int>> happinessMap) {
  int happiness = 0;
  final n = arrangement.length;
  for (int i = 0; i < n; i++) {
    final left = (i + n - 1) % n;
    final right = (i + 1) % n;
    happiness += happinessMap[arrangement[i]]![arrangement[left]]!;
    happiness += happinessMap[arrangement[i]]![arrangement[right]]!;
  }
  return happiness;
}

void _swap(List<String> arr, int i, int j) {
  final temp = arr[i];
  arr[i] = arr[j];
  arr[j] = temp;
}