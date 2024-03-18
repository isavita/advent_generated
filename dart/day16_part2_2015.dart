import 'dart:io';

void main() {
  File file = new File('input.txt');
  String input = file.readAsStringSync();
  int res = auntSue(input);
  print(res);
}

Map<String, int> targetSue = {
  "children": 3,
  "cats": 7,
  "samoyeds": 2,
  "pomeranians": 3,
  "akitas": 0,
  "vizslas": 0,
  "goldfish": 5,
  "trees": 3,
  "cars": 2,
  "perfumes": 1,
};

int auntSue(String input) {
  List<String> lines = input.split("\n");
  
  for (String line in lines) {
    List<String> words = line.split(" ");
    String thing1 = words[2].replaceAll(":", "");
    String thing2 = words[4].replaceAll(":", "");
    String thing3 = words[6].replaceAll(":", "");
    int sueNum = int.parse(words[1].replaceAll(":", ""));
    int amount1 = int.parse(words[3].replaceAll(",", ""));
    int amount2 = int.parse(words[5].replaceAll(",", ""));
    int amount3 = int.parse(words[7]);
    
    Map<String, int> readingsMap = {
      thing1: amount1,
      thing2: amount2,
      thing3: amount3,
    };
    
    bool allRulesMatched = true;
    
    for (String check in ["cats", "trees"]) {
      if (readingsMap.containsKey(check)) {
        if (readingsMap[check]! <= targetSue[check]!) {
          allRulesMatched = false;
        }
        readingsMap.remove(check);
      }
    }
    
    for (String check in ["pomeranians", "goldfish"]) {
      if (readingsMap.containsKey(check)) {
        if (readingsMap[check]! >= targetSue[check]!) {
          allRulesMatched = false;
        }
        readingsMap.remove(check);
      }
    }
    
    for (String thing in readingsMap.keys) {
      if (targetSue[thing]! != readingsMap[thing]) {
        allRulesMatched = false;
      }
    }
    
    if (allRulesMatched) {
      return sueNum;
    }
  }
  
  throw Exception("Expect return from loop");
}