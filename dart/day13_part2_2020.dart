import 'dart:io';

List<List<int>> readBusIDs(String fileName) {
  File file = File(fileName);
  List<int> ids = [];
  List<int> offsets = [];

  List<String> lines = file.readAsLinesSync();
  List<String> busData = lines[1].split(',');

  for (int i = 0; i < busData.length; i++) {
    if (busData[i] != 'x') {
      ids.add(int.parse(busData[i]));
      offsets.add(i);
    }
  }

  return [ids, offsets];
}

List<int> extendedGCD(int a, int b) {
  if (a == 0) {
    return [0, 1];
  }
  List<int> temp = extendedGCD(b % a, a);
  int x1 = temp[0];
  int y1 = temp[1];
  int x = y1 - (b ~/ a) * x1;
  int y = x1;
  return [x, y];
}

int findEarliestTimestamp(List<int> ids, List<int> offsets) {
  int N = 1;
  for (int id in ids) {
    N *= id;
  }

  int result = 0;
  for (int i = 0; i < ids.length; i++) {
    int ni = N ~/ ids[i];
    List<int> temp = extendedGCD(ni, ids[i]);
    int xi = temp[0];
    result += (-offsets[i] + ids[i]) % ids[i] * xi * ni;
  }

  return result % N;
}

void main() {
  List<List<int>> data = readBusIDs("input.txt");
  List<int> ids = data[0];
  List<int> offsets = data[1];

  int timestamp = findEarliestTimestamp(ids, offsets);
  print(timestamp);
}