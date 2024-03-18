import 'dart:io';

const iterations = 50;
const expandBy = 1;

void main() {
  final input = File('input.txt').readAsLinesSync();
  final algorithm = input.first;
  final image = _parseImage(input.skip(2).toList());

  var currentImage = image;
  for (var i = 0; i < iterations; i++) {
    currentImage = _enhanceImage(algorithm, currentImage, i % 2 == 1 && algorithm[0] == '#');
  }
  print(_countLitPixels(currentImage));
}

List<List<bool>> _parseImage(List<String> lines) {
  return lines.map((line) => line.split('').map((char) => char == '#').toList()).toList();
}

List<List<bool>> _enhanceImage(String algorithm, List<List<bool>> image, bool useInfiniteLit) {
  final newImage = List.generate(
    image.length + expandBy * 2,
    (_) => List.filled(image.first.length + expandBy * 2, false),
  );

  for (var y = -expandBy; y < image.length + expandBy; y++) {
    for (var x = -expandBy; x < image.first.length + expandBy; x++) {
      var index = 0;
      for (var dy = -1; dy <= 1; dy++) {
        for (var dx = -1; dx <= 1; dx++) {
          index <<= 1;
          final ny = y + dy;
          final nx = x + dx;
          if (ny >= 0 && ny < image.length && nx >= 0 && nx < image.first.length) {
            if (image[ny][nx]) {
              index |= 1;
            }
          } else if (useInfiniteLit) {
            index |= 1;
          }
        }
      }
      newImage[y + expandBy][x + expandBy] = algorithm[index] == '#';
    }
  }

  return newImage;
}

int _countLitPixels(List<List<bool>> image) {
  var count = 0;
  for (final row in image) {
    for (final pixel in row) {
      if (pixel) {
        count++;
      }
    }
  }
  return count;
}