import 'dart:io';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  
  List<List<int>> ranges = [];
  
  for (String line in input) {
    List<int> range = line.split('-').map(int.parse).toList();
    ranges.add(range);
  }
  
  ranges.sort((a, b) => a[0].compareTo(b[0]));
  
  int allowedIPs = 0;
  int currentMax = 0;
  
  for (List<int> range in ranges) {
    if (range[0] > currentMax + 1) {
      allowedIPs += range[0] - currentMax - 1;
    }
    currentMax = range[1] > currentMax ? range[1] : currentMax;
  }
  
  if (currentMax < 4294967295) {
    allowedIPs += 4294967295 - currentMax;
  }
  
  print(allowedIPs);
}