import 'dart:io';
import 'dart:math';

List<int> generateAddresses(String mask, int address) {
  List<int> floating = [];
  List<int> addresses = [];

  for (int i = 0; i < mask.length; i++) {
    if (mask[i] == '1') {
      address |= (1 << (35 - i));
    } else if (mask[i] == 'X') {
      floating.add(35 - i);
    }
  }

  int count = pow(2, floating.length).toInt();
  for (int i = 0; i < count; i++) {
    int modAddress = address;
    for (int j = 0; j < floating.length; j++) {
      if ((i & (1 << j)) == 0) {
        modAddress &= ~(1 << floating[j]);
      } else {
        modAddress |= (1 << floating[j]);
      }
    }
    addresses.add(modAddress);
  }

  return addresses;
}

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();
  String mask = '';
  Map<int, int> mem = {};

  RegExp reMem = RegExp(r'mem\[(\d+)] = (\d+)');

  for (String line in lines) {
    if (line.startsWith('mask = ')) {
      mask = line.substring(7);
    } else {
      RegExpMatch? matches = reMem.firstMatch(line);
      if (matches != null) {
        int address = int.parse(matches.group(1)!);
        int value = int.parse(matches.group(2)!);
        List<int> addresses = generateAddresses(mask, address);
        for (int addr in addresses) {
          mem[addr] = value;
        }
      }
    }
  }

  int sum = mem.values.reduce((value, element) => value + element);
  print(sum);
}