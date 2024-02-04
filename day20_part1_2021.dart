import 'dart:io';

void main() {
  var lines = File('input.txt').readAsLinesSync();
  var algorithm = lines[0];
  var image = lines.sublist(2).map((line) => line.runes.toList()).toList();
  image = enhanceImage(image, algorithm, 2);
  print(countLitPixels(image));
}

List<List<int>> enhanceImage(List<List<int>> image, String algorithm, int times) {
  for (var i = 0; i < times; i++) {
    image = applyAlgorithm(image, algorithm, i % 2 == 1 && algorithm[0] == '#');
  }
  return image;
}

List<List<int>> applyAlgorithm(List<List<int>> image, String algorithm, bool flip) {
  var enhancedImage = List.generate(image.length + 2, (index) => List.filled(image[0].length + 2, 0));
  for (var i = 0; i < enhancedImage.length; i++) {
    for (var j = 0; j < enhancedImage[i].length; j++) {
      var index = calculateIndex(i - 1, j - 1, image, flip);
      enhancedImage[i][j] = algorithm.codeUnitAt(index);
    }
  }
  return enhancedImage;
}

int calculateIndex(int i, int j, List<List<int>> image, bool flip) {
  var index = 0;
  for (var di = -1; di <= 1; di++) {
    for (var dj = -1; dj <= 1; dj++) {
      index <<= 1;
      if (i + di >= 0 && i + di < image.length && j + dj >= 0 && j + dj < image[0].length) {
        if (image[i + di][j + dj] == '#'.codeUnitAt(0)) {
          index |= 1;
        }
      } else if (flip) {
        index |= 1;
      }
    }
  }
  return index;
}

int countLitPixels(List<List<int>> image) {
  var count = 0;
  for (var row in image) {
    for (var pixel in row) {
      if (pixel == '#'.codeUnitAt(0)) {
        count++;
      }
    }
  }
  return count;
}