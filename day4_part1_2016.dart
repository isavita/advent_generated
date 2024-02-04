import 'dart:io';

void main() {
  var file = File('input.txt');
  var sumOfSectorIDs = 0;

  file.readAsLinesSync().forEach((line) {
    if (isRealRoom(line)) {
      sumOfSectorIDs += getSectorID(line);
    }
  });

  print(sumOfSectorIDs);
}

bool isRealRoom(String room) {
  var parts = room.split("[");
  var checksum = parts[1].replaceAll("]", "");
  var encryptedName = parts[0].split("-");
  encryptedName = encryptedName.sublist(0, encryptedName.length - 1);

  var letterCounts = Map<String, int>();
  for (var part in encryptedName) {
    for (var letter in part.runes) {
      letterCounts[String.fromCharCode(letter)] =
          (letterCounts[String.fromCharCode(letter)] ?? 0) + 1;
    }
  }

  var counts = letterCounts.entries.toList();
  counts.sort((a, b) {
    if (a.value == b.value) {
      return a.key.compareTo(b.key);
    }
    return b.value.compareTo(a.value);
  });

  for (var i = 0; i < checksum.length; i++) {
    if (checksum.codeUnitAt(i) != counts[i].key.runes.first) {
      return false;
    }
  }

  return true;
}

int getSectorID(String room) {
  var parts = room.split("-");
  var sectorIDPart = parts[parts.length - 1];
  var sectorID = int.parse(sectorIDPart.split("[")[0]);
  return sectorID;
}