
import 'dart:io';

class DirectoryNode {
  String name;
  List<DirectoryNode> subdirectories = [];
  List<int> fileSizes = [];

  DirectoryNode(this.name);

  int getTotalSize() {
    int total = fileSizes.fold(0, (sum, size) => sum + size);
    for (var subdir in subdirectories) {
      total += subdir.getTotalSize();
    }
    return total;
  }
}

void main() async {
  // Read input from the file
  final input = await File('input.txt').readAsLines();
  
  DirectoryNode root = DirectoryNode('/');
  DirectoryNode currentDir = root;
  List<DirectoryNode> directoryStack = [root];

  for (var line in input) {
    if (line.startsWith('\$ cd ')) {
      String arg = line.substring(5);
      if (arg == '..') {
        directoryStack.removeLast();
        currentDir = directoryStack.last;
      } else if (arg == '/') {
        currentDir = root;
        directoryStack = [root];
      } else {
        // Find or create the new directory
        DirectoryNode newDir = currentDir.subdirectories.firstWhere(
            (dir) => dir.name == arg,
            orElse: () {
              DirectoryNode dir = DirectoryNode(arg);
              currentDir.subdirectories.add(dir);
              return dir;
            });
        currentDir = newDir;
        directoryStack.add(newDir);
      }
    } else if (line.startsWith('\$ ls')) {
      // Do nothing, just a command
    } else {
      // This is a file or directory listing
      if (line.startsWith('dir ')) {
        // It's a directory, we already handle it in cd command
        continue;
      } else {
        // It's a file
        var parts = line.split(' ');
        int size = int.parse(parts[0]);
        currentDir.fileSizes.add(size);
      }
    }
  }

  // Calculate the total sizes of directories
  int sumOfSmallDirectories = 0;

  void calculateSizes(DirectoryNode dir) {
    int size = dir.getTotalSize();
    if (size <= 100000) {
      sumOfSmallDirectories += size;
    }
    for (var subdir in dir.subdirectories) {
      calculateSizes(subdir);
    }
  }

  calculateSizes(root);
  
  // Print the result
  print(sumOfSmallDirectories);
}
