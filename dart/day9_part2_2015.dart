import 'dart:io';

void main() {
  final distances = readAndParseInput("input.txt");
  final locations = getUniqueLocations(distances);
  final maxDistance = findLongestRoute(locations, distances);
  print(maxDistance);
}

Map<String, Map<String, int>> readAndParseInput(String filename) {
  final file = File(filename);
  final distances = <String, Map<String, int>>{};

  file.readAsLinesSync().forEach((line) {
    final parts = line.split(' ');
    if (parts.length != 5) return;

    final from = parts[0];
    final to = parts[2];
    final dist = int.parse(parts[4]);

    distances.putIfAbsent(from, () => {});
    distances[from]!.putIfAbsent(to, () => dist);

    distances.putIfAbsent(to, () => {});
    distances[to]!.putIfAbsent(from, () => dist);
  });

  return distances;
}

List<String> getUniqueLocations(Map<String, Map<String, int>> distances) {
  final locationSet = <String>{};
  
  distances.keys.forEach((from) {
    locationSet.add(from);
    distances[from]!.keys.forEach((to) {
      locationSet.add(to);
    });
  });

  return locationSet.toList();
}

int findLongestRoute(List<String> locations, Map<String, Map<String, int>> distances) {
  var maxDistance = 0;
  permute(locations, 0, distances, (dist) {
    if (dist > maxDistance) {
      maxDistance = dist;
    }
  });
  return maxDistance;
}

void permute(List<String> arr, int i, Map<String, Map<String, int>> distances, void Function(int) updateBestDistance) {
  if (i == arr.length) {
    final dist = calculateRouteDistance(arr, distances);
    updateBestDistance(dist);
    return;
  }
  
  for (var j = i; j < arr.length; j++) {
    final temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
    permute(arr, i + 1, distances, updateBestDistance);
    arr[j] = arr[i];
    arr[i] = temp;
  }
}

int calculateRouteDistance(List<String> route, Map<String, Map<String, int>> distances) {
  var sum = 0;
  for (var i = 0; i < route.length - 1; i++) {
    sum += distances[route[i]]![route[i + 1]]!;
  }
  return sum;
}