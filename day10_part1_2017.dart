
import 'dart:io';

void main() {
  List<int> list = List.generate(256, (index) => index);
  int currentPosition = 0;
  int skipSize = 0;

  List<int> lengths = File('input.txt').readAsStringSync().trim().split(',').map(int.parse).toList();

  for (int length in lengths) {
    if (length > list.length) continue;

    List<int> sublist = [];
    for (int i = 0; i < length; i++) {
      sublist.add(list[(currentPosition + i) % list.length]);
    }

    sublist = sublist.reversed.toList();

    for (int i = 0; i < length; i++) {
      list[(currentPosition + i) % list.length] = sublist[i];
    }

    currentPosition = (currentPosition + length + skipSize) % list.length;
    skipSize++;
  }

  print(list[0] * list[1]);
}
