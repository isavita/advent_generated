import 'dart:io';

const int Size = 119315717514047;

class Deck {
  int step = 1;
  int direction = 1;
  int top = 0;

  void dealIntoNewStack() {
    top = (top - direction * step + Size) % Size;
    direction *= -1;
  }

  void cutN(int n) {
    top = (top + (direction * step * n) + Size) % Size;
  }

  void dealWithIncrementN(int n) {
    int inv = modinv(n, Size);
    step *= inv;
    top *= inv;
  }

  int pick(int n) {
    int current = top;
    for (int i = 0; i < n; i++) {
      current = ((current + direction * step) % Size + Size) % Size;
    }
    return current;
  }
}

Deck newDeck() {
  return Deck();
}

void main() {
  BigInt size = BigInt.from(Size);
  BigInt iter = BigInt.parse('101741582076661');
  BigInt offset = BigInt.zero;
  BigInt increment = BigInt.one;

  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    if (line == 'deal into new stack') {
      increment *= BigInt.from(-1);
      offset += increment;
    } else if (line.startsWith('cut')) {
      int n = int.parse(line.split(' ')[1]);
      offset += BigInt.from(n) * increment;
    } else if (line.startsWith('deal with increment')) {
      int n = int.parse(line.split(' ')[3]);
      increment *= BigInt.from(n).modPow(size - BigInt.two, size);
    }
  }

  BigInt finalIncr = increment.modPow(iter, size);

  BigInt finalOffs = increment.modPow(iter, size);
  finalOffs = BigInt.one - finalOffs;
  BigInt invmod = (BigInt.one - increment).modPow(size - BigInt.two, size);
  finalOffs *= invmod;
  finalOffs *= offset;

  BigInt answer = BigInt.from(2020) * finalIncr;
  answer += finalOffs;
  answer %= size;

  print(answer);
}

List<int> egcd(int a, int b) {
  if (a == 0) {
    return [b, 0, 1];
  }
  List<int> res = egcd(b % a, a);
  int gcd = res[0];
  int y = res[1];
  int x = res[2];
  return [gcd, x - (b ~/ a) * y, y];
}

int modinv(int a, int m) {
  List<int> res = egcd(a, m);
  int g = res[0];
  int x = res[1];
  if (g != 1) {
    throw Exception('modular inverse does not exist');
  }
  if (x < 0) {
    x += m;
  }
  return x % m;
}