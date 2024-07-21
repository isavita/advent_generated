
import 'dart:io';

void main() async {
  final imageData = (await File('input.txt').readAsString()).trim();
  const width = 25, height = 6;
  final layerSize = width * height;
  final finalImage = List.filled(layerSize, '2');

  for (var i = 0; i < imageData.length; i += layerSize) {
    final layer = imageData.substring(i, (i + layerSize).clamp(0, imageData.length));

    for (var j = 0; j < layer.length; j++) {
      if (finalImage[j] == '2') {
        finalImage[j] = layer[j];
      }
    }
  }

  print("Decoded image:");
  for (var i = 0; i < height; i++) {
    for (var j = 0; j < width; j++) {
      final pixel = finalImage[i * width + j];
      stdout.write(pixel == '0' ? ' ' : '#');
    }
    print('');
  }
}
