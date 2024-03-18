import 'dart:io';
import 'dart:math' as math;

class LLNode {
  int elfNum;
  int presents;
  LLNode? next;

  LLNode(this.elfNum, this.presents);
}

int elephant(String input) {
  int startingElves = int.parse(input);
  LLNode root = LLNode(1, 1);
  LLNode iter = root;
  for (int i = 2; i <= startingElves; i++) {
    iter.next = LLNode(i, 1);
    iter = iter.next!;
  }
  iter.next = root;

  bool isOddLength = startingElves % 2 == 1;
  LLNode beforeAcross = root;
  for (int i = 0; i < startingElves ~/ 2 - 1; i++) {
    beforeAcross = beforeAcross.next!;
  }

  while (root.next != root) {
    root.presents += beforeAcross.next!.presents;

    beforeAcross.next = beforeAcross.next!.next;

    if (isOddLength) {
      beforeAcross = beforeAcross.next!;
    }
    isOddLength = !isOddLength;
    root = root.next!;
  }

  return root.elfNum;
}

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  print(elephant(input));
}