
import 'dart:io';
import 'dart:math';

const int mod = 1 << 24;
const int numSteps = 2000;

int nextSecret(int s) {
  int x = s * 64;
  s ^= x;
  s &= mod - 1;
  x = s ~/ 32;
  s ^= x;
  s &= mod - 1;
  x = s * 2048;
  s ^= x;
  s &= mod - 1;
  return s;
}

int encodeChange4(int c1, int c2, int c3, int c4) {
  return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
}

void main() {
  List<int> initials = [];
  File('input.txt').readAsLinesSync().forEach((line) {
    if (line.isNotEmpty) {
      initials.add(int.parse(line));
    }
  });

  List<List<int>> buyers = [];
  for (int initVal in initials) {
    List<int> prices = List<int>.filled(numSteps + 1, 0);
    int s = initVal;
    for (int j = 0; j <= numSteps; j++) {
      prices[j] = s % 10;
      if (j < numSteps) {
        s = nextSecret(s);
      }
    }
    List<int> changes = List<int>.filled(numSteps, 0);
    for (int j = 0; j < numSteps; j++) {
      changes[j] = prices[j + 1] - prices[j];
    }
    buyers.add([...prices, ...changes]);
  }

  const int patternCount = 19 * 19 * 19 * 19;
  List<int> globalSum = List<int>.filled(patternCount, 0);

  for (List<int> b in buyers) {
    List<int> localPrice = List<int>.filled(patternCount, -1);
    for (int i = 0; i + 3 < numSteps; i++) {
      int c1 = b[i + numSteps + 1], c2 = b[i + numSteps + 2], c3 = b[i + numSteps + 3], c4 = b[i + numSteps + 4];
      if (c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 || c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9) {
        continue;
      }
      int idx = encodeChange4(c1, c2, c3, c4);
      if (localPrice[idx] < 0) {
        localPrice[idx] = b[i + 4];
      }
    }
    for (int idx = 0; idx < patternCount; idx++) {
      if (localPrice[idx] >= 0) {
        globalSum[idx] += localPrice[idx];
      }
    }
  }

  int ans = globalSum.reduce(max);
  print(ans);
}
