
import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync();
  final diskMap = parseDiskMap(input);
  final compactedDisk = compactDisk(diskMap);
  final checksum = calculateChecksum(compactedDisk);
  print(checksum);
}

List<int> parseDiskMap(String input) {
  final diskMap = <int>[];
  for (int i = 0; i < input.length; i++) {
    diskMap.add(int.parse(input[i]));
  }
  return diskMap;
}

List<int> compactDisk(List<int> diskMap) {
  final blocks = <int>[];
  final fileIds = <int>[];
  int fileId = 0;
  bool isFile = true;

  for (int i = 0; i < diskMap.length; i++) {
    final length = diskMap[i];
    if (isFile) {
      for (int j = 0; j < length; j++) {
        blocks.add(fileId);
      }
      fileIds.add(fileId);
      fileId++;
    } else {
      for (int j = 0; j < length; j++) {
        blocks.add(-1);
      }
    }
    isFile = !isFile;
  }

  while (true) {
    final lastFileIndex = blocks.lastIndexWhere((b) => b != -1);
    if (lastFileIndex == -1) break;

    final lastFileId = blocks[lastFileIndex];

    final firstFreeIndex = blocks.indexWhere((b) => b == -1);

    if (firstFreeIndex == -1 || firstFreeIndex > lastFileIndex) break;
        
    blocks[firstFreeIndex] = lastFileId;
    blocks[lastFileIndex] = -1;
  }

  return blocks;
}

int calculateChecksum(List<int> compactedDisk) {
  int checksum = 0;
  for (int i = 0; i < compactedDisk.length; i++) {
    if (compactedDisk[i] != -1) {
      checksum += i * compactedDisk[i];
    }
  }
  return checksum;
}
