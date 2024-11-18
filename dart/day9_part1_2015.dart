
import 'dart:io';
import 'dart:math';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final distances = parseInput(input);
  final locations = getUniqueLocations(distances);
  final minDistance = findShortestRoute(locations, distances);
  print(minDistance);
}

Map<String, Map<String, int>> parseInput(List<String> lines) {
  final distances = <String, Map<String, int>>{};
  
  for (final line in lines) {
    final parts = line.split(' ');
    if (parts.length != 5) continue;
    
    final from = parts[0];
    final to = parts[2];
    final dist = int.parse(parts[4]);
    
    distances.putIfAbsent(from, () => {});
    distances.putIfAbsent(to, () => {});
    
    distances[from]![to] = dist;
    distances[to]![from] = dist;
  }
  
  return distances;
}

List<String> getUniqueLocations(Map<String, Map<String, int>> distances) {
  final locationSet = <String>{};
  
  for (final from in distances.keys) {
    locationSet.add(from);
    locationSet.addAll(distances[from]!.keys);
  }
  
  return locationSet.toList();
}

int findShortestRoute(List<String> locations, Map<String, Map<String, int>> distances) {
  var minDistance = -1;
  
  void permute(List<String> arr, int i) {
    if (i > arr.length) return;
    
    if (i == arr.length) {
      final dist = calculateRouteDistance(arr, distances);
      if (minDistance == -1 || dist < minDistance) {
        minDistance = dist;
      }
      return;
    }
    
    for (var j = i; j < arr.length; j++) {
      final temp = arr[i];
      arr[i] = arr[j];
      arr[j] = temp;
      
      permute(arr, i + 1);
      
      arr[j] = arr[i];
      arr[i] = temp;
    }
  }
  
  permute(locations, 0);
  return minDistance;
}

int calculateRouteDistance(List<String> route, Map<String, Map<String, int>> distances) {
  var sum = 0;
  for (var i = 0; i < route.length - 1; i++) {
    sum += distances[route[i]]![route[i + 1]]!;
  }
  return sum;
}
