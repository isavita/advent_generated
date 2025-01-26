
import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  print('Part 1: ${calculateChecksum(input, false)}');
  print('Part 2: ${calculateChecksum(input, true)}');
}

int calculateChecksum(String input, bool moveFiles) {
  final segments = parseInput(input);
  final disk = createDisk(segments);
  final compactedDisk = moveFiles ? compactDiskByFiles(disk) : compactDiskByBlocks(disk);
  return calculateChecksumFromDisk(compactedDisk);
}

List<Segment> parseInput(String input) {
  final segments = <Segment>[];
  for (int i = 0; i < input.length; i += 2) {
    final fileLength = int.parse(input[i]);
    final freeLength = i + 1 < input.length ? int.parse(input[i + 1]) : 0;
    segments.add(Segment(fileLength, freeLength));
  }
  return segments;
}

List<String> createDisk(List<Segment> segments) {
  final disk = <String>[];
  int fileId = 0;
  for (final segment in segments) {
    for (int i = 0; i < segment.fileLength; i++) {
      disk.add('$fileId');
    }
    for (int i = 0; i < segment.freeLength; i++) {
      disk.add('.');
    }
    if (segment.fileLength > 0) fileId++;
  }
  return disk;
}

List<String> compactDiskByBlocks(List<String> disk) {
  final workingDisk = List<String>.from(disk);
  int current = workingDisk.length - 1;
  while (current >= 0) {
    if (workingDisk[current] == '.') {
      current--;
      continue;
    }
    int freeSpaceIndex = -1;
      for (int i = 0; i < current; i++) {
        if (workingDisk[i] == '.') {
          freeSpaceIndex = i;
          break;
        }
      }
      
      if(freeSpaceIndex == -1)
        break;

      workingDisk[freeSpaceIndex] = workingDisk[current];
      workingDisk[current] = '.';
      
    
    current--;
  }
  return workingDisk;
}


List<String> compactDiskByFiles(List<String> disk) {
  final workingDisk = List<String>.from(disk);
  final filePositions = <int, List<int>>{};
  for (int i = 0; i < workingDisk.length; i++) {
    if (workingDisk[i] != '.') {
      filePositions.putIfAbsent(int.parse(workingDisk[i]), () => []).add(i);
    }
  }
  
  final sortedFileIds = filePositions.keys.toList()..sort((a,b) => b.compareTo(a));
  for (final fileId in sortedFileIds) {
    final fileBlockPositions = filePositions[fileId]!;
    final fileLength = fileBlockPositions.length;

    int freeSpaceIndex = -1;
    int freeSpaceLength = 0;
    for (int i = 0; i < fileBlockPositions[0]; i++){
      if (workingDisk[i] == '.'){
        if (freeSpaceIndex == -1){
          freeSpaceIndex = i;
        }
        freeSpaceLength++;
      } else {
        freeSpaceIndex = -1;
        freeSpaceLength = 0;
      }
       
      if(freeSpaceLength == fileLength)
        break;
    }
      
      if (freeSpaceIndex != -1 && freeSpaceLength >= fileLength){
        for(int i = 0; i < fileLength; i++){
            workingDisk[freeSpaceIndex + i] = workingDisk[fileBlockPositions[i]];
            workingDisk[fileBlockPositions[i]] = '.';
        }
        
      }
  }

  return workingDisk;
}


int calculateChecksumFromDisk(List<String> disk) {
  int checksum = 0;
  for (int i = 0; i < disk.length; i++) {
    if (disk[i] != '.') {
      checksum += i * int.parse(disk[i]);
    }
  }
  return checksum;
}


class Segment {
  int fileLength;
  int freeLength;

  Segment(this.fileLength, this.freeLength);
}
