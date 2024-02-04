import 'dart:io';

void main() {
  var file = File('input.txt');
  var contents = file.readAsStringSync().trim();

  int width = 25;
  int height = 6;
  int layerSize = width * height;

  int minZeros = layerSize + 1;
  int result = 0;

  for (int i = 0; i < contents.length; i += layerSize) {
    var layer = contents.substring(i, i + layerSize);
    int zeroCount = 0;
    int oneCount = 0;
    int twoCount = 0;

    for (var pixel in layer.runes) {
      switch (String.fromCharCode(pixel)) {
        case '0':
          zeroCount++;
          break;
        case '1':
          oneCount++;
          break;
        case '2':
          twoCount++;
          break;
      }
    }

    if (zeroCount < minZeros) {
      minZeros = zeroCount;
      result = oneCount * twoCount;
    }
  }

  print(result);
}