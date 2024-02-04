import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  List<List<int>> ingredients = [];
  
  for (String line in lines) {
    List<String> parts = line.split(' ');
    ingredients.add([
      int.parse(parts[2].substring(0, parts[2].length - 1)),
      int.parse(parts[4].substring(0, parts[4].length - 1)),
      int.parse(parts[6].substring(0, parts[6].length - 1)),
      int.parse(parts[8].substring(0, parts[8].length - 1)),
      int.parse(parts[10])
    ]);
  }
  
  int maxScore = 0;
  
  for (int i = 0; i <= 100; i++) {
    for (int j = 0; j <= 100 - i; j++) {
      for (int k = 0; k <= 100 - i - j; k++) {
        int l = 100 - i - j - k;
        
        int capacity = ingredients[0][0] * i + ingredients[1][0] * j + ingredients[2][0] * k + ingredients[3][0] * l;
        int durability = ingredients[0][1] * i + ingredients[1][1] * j + ingredients[2][1] * k + ingredients[3][1] * l;
        int flavor = ingredients[0][2] * i + ingredients[1][2] * j + ingredients[2][2] * k + ingredients[3][2] * l;
        int texture = ingredients[0][3] * i + ingredients[1][3] * j + ingredients[2][3] * k + ingredients[3][3] * l;
        
        if (capacity <= 0 || durability <= 0 || flavor <= 0 || texture <= 0) {
          continue;
        }
        
        int score = capacity * durability * flavor * texture;
        
        if (score > maxScore) {
          maxScore = score;
        }
      }
    }
  }
  
  print(maxScore);
}