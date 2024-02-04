
import 'dart:io';

String hexToBin(String hex) {
  String bin = '';
  for (int i = 0; i < hex.length; i++) {
    int b = int.parse(hex[i], radix: 16);
    bin += b.toRadixString(2).padLeft(4, '0');
  }
  return bin;
}

List<int> parsePacket(String binStr, int idx) {
  int version = int.parse(binStr[idx]) << 2 | int.parse(binStr[idx + 1]) << 1 | int.parse(binStr[idx + 2]);
  int typeID = int.parse(binStr[idx + 3]) << 2 | int.parse(binStr[idx + 4]) << 1 | int.parse(binStr[idx + 5]);
  idx += 6;

  if (typeID == 4) {
    int value = 0;
    while (binStr[idx] == '1') {
      value = (value << 4) | int.parse(binStr[idx + 1]) << 3 | int.parse(binStr[idx + 2]) << 2 | int.parse(binStr[idx + 3]) << 1 | int.parse(binStr[idx + 4]);
      idx += 5;
    }
    value = (value << 4) | int.parse(binStr[idx + 1]) << 3 | int.parse(binStr[idx + 2]) << 2 | int.parse(binStr[idx + 3]) << 1 | int.parse(binStr[idx + 4]);
    idx += 5;
    return [version, idx, value];
  }

  int lengthTypeID = int.parse(binStr[idx]);
  idx++;
  int numSubPackets = 0;
  int subPacketLength = 0;

  if (lengthTypeID == 0) {
    subPacketLength = 0;
    for (int i = 0; i < 15; i++) {
      subPacketLength = (subPacketLength << 1) | int.parse(binStr[idx]);
      idx++;
    }
  } else {
    numSubPackets = 0;
    for (int i = 0; i < 11; i++) {
      numSubPackets = (numSubPackets << 1) | int.parse(binStr[idx]);
      idx++;
    }
  }

  List<int> values = [];
  while (true) {
    if (lengthTypeID == 0 && subPacketLength == 0) {
      break;
    }
    if (lengthTypeID == 1 && numSubPackets == 0) {
      break;
    }
    List<int> result = parsePacket(binStr, idx);
    values.add(result[2]);

    if (lengthTypeID == 0) {
      subPacketLength -= result[1] - idx;
    } else {
      numSubPackets--;
    }
    idx = result[1];
  }

  int result = 0;
  switch (typeID) {
    case 0:
      result = 0;
      for (int value in values) {
        result += value;
      }
      break;
    case 1:
      result = 1;
      for (int value in values) {
        result *= value;
      }
      break;
    case 2:
      result = values[0];
      for (int value in values) {
        if (value < result) {
          result = value;
        }
      }
      break;
    case 3:
      result = values[0];
      for (int value in values) {
        if (value > result) {
          result = value;
        }
      }
      break;
    case 5:
      result = 0;
      if (values[0] > values[1]) {
        result = 1;
      }
      break;
    case 6:
      result = 0;
      if (values[0] < values[1]) {
        result = 1;
      }
      break;
    case 7:
      result = 0;
      if (values[0] == values[1]) {
        result = 1;
      }
      break;
    default:
      throw 'Unknown typeID';
  }

  return [version, idx, result];
}

void main() {
  String hexStr = File('input.txt').readAsStringSync().trim();
  String binStr = hexToBin(hexStr);
  List<int> result = parsePacket(binStr, 0);
  print(result[2]);
}
