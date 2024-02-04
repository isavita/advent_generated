import 'dart:io';

void main() {
  var file = new File('input.txt');
  var totalDiff = 0;

  file.readAsLinesSync().forEach((line) {
    var originalLength = line.length;
    var encodedLength = calculateEncodedLength(line);
    totalDiff += encodedLength - originalLength;
  });

  print(totalDiff);
}

int calculateEncodedLength(String s) {
  var encoded = '"';
  s.runes.forEach((ch) {
    if (ch == 92 || ch == 34) {
      encoded += '\\';
    }
    encoded += String.fromCharCode(ch);
  });
  encoded += '"';
  return encoded.length;
}