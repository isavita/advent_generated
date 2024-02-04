
import 'dart:io';

void main() {
  var file = File('input.txt');
  var seatIDs = <int>[];

  file.readAsLinesSync().forEach((pass) {
    pass = pass.replaceAll('F', '0').replaceAll('B', '1').replaceAll('L', '0').replaceAll('R', '1');
    var seatID = decode(pass);
    seatIDs.add(seatID);
  });

  seatIDs.sort();

  for (var i = 0; i < seatIDs.length - 1; i++) {
    if (seatIDs[i + 1] != seatIDs[i] + 1) {
      print(seatIDs[i] + 1);
      break;
    }
  }
}

int decode(String pass) {
  var row = binaryToInt(pass.substring(0, 7));
  var column = binaryToInt(pass.substring(7));
  return row * 8 + column;
}

int binaryToInt(String binaryStr) {
  var result = 0;
  for (var i = 0; i < binaryStr.length; i++) {
    if (binaryStr[i] == '1') {
      result |= 1 << (binaryStr.length - i - 1);
    }
  }
  return result;
}
