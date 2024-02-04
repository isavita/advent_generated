import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    if (isRealRoom(line)) {
      String decryptedName = decryptName(line);
      if (decryptedName.contains("northpole object")) {
        print(getSectorID(line));
        break;
      }
    }
  }
}

bool isRealRoom(String room) {
  List<String> parts = room.split("[");
  String checksum = parts[1].replaceAll("]", "");
  List<String> encryptedName = parts[0].split("-");
  encryptedName = encryptedName.sublist(0, encryptedName.length - 1);

  Map<String, int> letterCounts = {};
  for (String part in encryptedName) {
    for (int i = 0; i < part.length; i++) {
      String letter = part[i];
      letterCounts[letter] = (letterCounts[letter] ?? 0) + 1;
    }
  }

  List<MapEntry<String, int>> counts = letterCounts.entries.toList();
  counts.sort((a, b) {
    if (a.value == b.value) {
      return a.key.compareTo(b.key);
    }
    return b.value.compareTo(a.value);
  });

  for (int i = 0; i < checksum.length; i++) {
    if (checksum.codeUnitAt(i) != counts[i].key.codeUnitAt(0)) {
      return false;
    }
  }

  return true;
}

int getSectorID(String room) {
  List<String> parts = room.split("-");
  String sectorIDPart = parts[parts.length - 1];
  int sectorID = int.parse(sectorIDPart.split("[")[0]);
  return sectorID;
}

String decryptName(String room) {
  List<String> parts = room.split("-");
  String sectorIDPart = parts[parts.length - 1];
  int sectorID = int.parse(sectorIDPart.split("[")[0]);
  StringBuffer decryptedName = StringBuffer();

  for (String part in parts.sublist(0, parts.length - 1)) {
    for (int i = 0; i < part.length; i++) {
      String letter = part[i];
      if (letter == '-') {
        decryptedName.write(' ');
      } else {
        int shiftedLetter = 'a'.codeUnitAt(0) + ((letter.codeUnitAt(0) - 'a'.codeUnitAt(0) + sectorID) % 26);
        decryptedName.write(String.fromCharCode(shiftedLetter));
      }
    }
    decryptedName.write(' ');
  }

  return decryptedName.toString().trim();
}