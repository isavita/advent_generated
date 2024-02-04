import 'dart:io';

void main() {
  final favoriteNumber = int.parse(File('input.txt').readAsStringSync().trim());

  int countBits(int n) {
    int count = 0;
    while (n > 0) {
      count += n & 1;
      n >>= 1;
    }
    return count;
  }

  bool isWall(int x, int y) {
    int sum = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
    return countBits(sum) % 2 != 0;
  }

  int shortestPath(int targetX, int targetY) {
    List<List<int>> visited = List.generate(50, (_) => List.filled(50, 0));
    List<List<int>> queue = [[1, 1, 0]];
    List<List<int>> directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    while (queue.isNotEmpty) {
      List<int> current = queue.removeAt(0);
      int x = current[0];
      int y = current[1];
      int steps = current[2];

      if (x == targetX && y == targetY) {
        return steps;
      }

      visited[y][x] = 1;

      for (List<int> dir in directions) {
        int newX = x + dir[0];
        int newY = y + dir[1];

        if (newX >= 0 && newY >= 0 && !isWall(newX, newY) && visited[newY][newX] == 0) {
          queue.add([newX, newY, steps + 1]);
          visited[newY][newX] = 1;
        }
      }
    }

    return -1;
  }

  print(shortestPath(31, 39));
}