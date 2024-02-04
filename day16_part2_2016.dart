import 'dart:io';

const diskLength = 35651584;

void main() {
  final initialState = readInitialState('input.txt');
  final data = generateData(initialState, diskLength);
  final checksum = calculateChecksum(data);
  print('Checksum: $checksum');
}

String readInitialState(String filename) {
  final file = File(filename);
  final contents = file.readAsStringSync();
  return contents.trim();
}

String generateData(String initialState, int length) {
  var data = initialState;
  while (data.length < length) {
    var b = StringBuffer();
    for (var i = data.length - 1; i >= 0; i--) {
      if (data[i] == '0') {
        b.write('1');
      } else {
        b.write('0');
      }
    }
    data = data + '0' + b.toString();
  }
  return data.substring(0, length);
}

String calculateChecksum(String data) {
  while (data.length % 2 == 0) {
    var b = StringBuffer();
    for (var i = 0; i < data.length; i += 2) {
      if (data[i] == data[i + 1]) {
        b.write('1');
      } else {
        b.write('0');
      }
    }
    data = b.toString();
  }
  return data;
}