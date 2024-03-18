import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  int ans = jumbledSevenSegment(lines);
  print(ans);
}

int jumbledSevenSegment(List<String> lines) {
  List<List<String>> parsedInput = [];

  for (int i = 0; i < lines.length; i++) {
    List<String> parts = RegExp(r'([a-g]+)').allMatches(lines[i]).map((m) => m.group(0)!).toList();

    if (parts.length != 14) {
      throw Exception("should be 14 parts in each input line, got ${parts.length} for line $i");
    }

    List<String> fourteen = parts.map((v) => alphabetizeString(v)).toList();
    parsedInput.add(fourteen);
  }

  int ans = 0;
  List<String?> indexToCharacters = List.filled(10, "");

  for (List<String> set in parsedInput) {
    List<String> workingSet = set.sublist(0, 10);
    List<int> killIndices = [];

    for (int i = 0; i < workingSet.length; i++) {
      switch (workingSet[i].length) {
        case 2:
          indexToCharacters[1] = workingSet[i];
          killIndices.add(i);
          break;
        case 4:
          indexToCharacters[4] = workingSet[i];
          killIndices.add(i);
          break;
        case 3:
          indexToCharacters[7] = workingSet[i];
          killIndices.add(i);
          break;
        case 7:
          indexToCharacters[8] = workingSet[i];
          killIndices.add(i);
          break;
      }
    }

    workingSet = removeSliceIndices(workingSet, killIndices);

    List<String> zeroThreeOrNine = [];
    killIndices = [];
    for (int i = 0; i < workingSet.length; i++) {
      if (checkStringOverlap(workingSet[i], indexToCharacters[1]!)) {
        zeroThreeOrNine.add(workingSet[i]);
        killIndices.add(i);
      }
    }

    if (zeroThreeOrNine.length != 3) {
      throw Exception("one three or nine does not have three matches: got ${zeroThreeOrNine.length}");
    }

    for (int i = 0; i < zeroThreeOrNine.length; i++) {
      if (zeroThreeOrNine[i].length == 5) {
        indexToCharacters[3] = zeroThreeOrNine[i];
        zeroThreeOrNine.removeAt(i);
        break;
      }
    }

    for (int i = 0; i < zeroThreeOrNine.length; i++) {
      if (checkStringOverlap(zeroThreeOrNine[i], indexToCharacters[4]!)) {
        indexToCharacters[9] = zeroThreeOrNine[i];
        zeroThreeOrNine.removeAt(i);
      }
    }

    indexToCharacters[0] = zeroThreeOrNine[0]!;

    workingSet = removeSliceIndices(workingSet, killIndices);

    if (workingSet.length != 3) {
      throw Exception("expected length of 3 at this stage, got ${workingSet.length}");
    }

    for (int i = 0; i < workingSet.length; i++) {
      if (workingSet[i].length == 6) {
        indexToCharacters[6] = workingSet[i];
        workingSet.removeAt(i);
      }
    }

    for (int i = 0; i < workingSet.length; i++) {
      if (checkStringOverlap(indexToCharacters[9]!, workingSet[i])) {
        indexToCharacters[5] = workingSet[i];
        workingSet.removeAt(i);
      }
    }

    if (workingSet.length != 1) {
      throw Exception("expected length of 1 at this stage, got ${workingSet.length}");
    }

    indexToCharacters[2] = workingSet[0]!;

    int num = 0;
    for (String out in set.sublist(10)) {
      for (int i = 0; i < indexToCharacters.length; i++) {
        if (out == indexToCharacters[i]) {
          num *= 10;
          num += i;
        }
      }
    }
    ans += num;
  }

  return ans;
}

List<String> removeSliceIndices(List<String> sli, List<int> indices) {
  Map<int, bool> m = {};
  for (int v in indices) {
    m[v] = true;
  }

  List<String> ans = [];
  for (int i = 0; i < sli.length; i++) {
    if (!m.containsKey(i)) {
      ans.add(sli[i]);
    }
  }
  return ans;
}

bool checkStringOverlap(String larger, String smaller) {
  if (larger.length < smaller.length) {
    String temp = larger;
    larger = smaller;
    smaller = temp;
  }

  Map<String, bool> largeMap = {};
  for (int i = 0; i < larger.length; i++) {
    largeMap[larger[i]] = true;
  }

  for (int i = 0; i < smaller.length; i++) {
    if (!largeMap.containsKey(smaller[i])) {
      return false;
    }
  }
  return true;
}

String alphabetizeString(String input) {
  List<String> chars = input.split('');
  chars.sort();
  return chars.join('');
}