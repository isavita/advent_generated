import 'dart:io';
import 'dart:math';

class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  static final Complex zero = Complex(0, 0);
  static final Complex one = Complex(1, 0);

  Complex operator +(Complex other) => Complex(real + other.real, imaginary + other.imaginary);
  Complex operator -(Complex other) => Complex(real - other.real, imaginary - other.imaginary);
  Complex operator *(Complex other) => Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real,
      );
  Complex operator /(Complex other) {
    final denom = other.real * other.real + other.imaginary * other.imaginary;
    return Complex(
      (real * other.real + imaginary * other.imaginary) / denom,
      (imaginary * other.real - real * other.imaginary) / denom,
    );
  }

  bool operator ==(Object other) =>
      other is Complex && real == other.real && imaginary == other.imaginary;
  int get hashCode => Object.hash(real, imaginary);
}

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  final garden = <Complex, bool>{};
  Complex start = Complex.zero;

  for (var y = 0; y < lines.length; y++) {
    for (var x = 0; x < lines[y].length; x++) {
      if (lines[y][x] != '#') {
        garden[Complex(x.toDouble(), y.toDouble())] = true;
      }
      if (lines[y][x] == 'S') {
        start = Complex(x.toDouble(), y.toDouble());
      }
    }
  }

  if (start == Complex.zero) {
    throw Exception('No start found!');
  }

  final maxSize = lines.length;
  final sum = calculateNumEnds(garden, start, 26501365, maxSize);
  print(sum);
}

Complex complexMod(Complex num, int mod) {
  if (num.real != num.real.toInt() || num.imaginary != num.imaginary.toInt()) {
    throw Exception('Complex number not integer!');
  }
  return Complex(
    ((num.real + 10 * mod) % mod).toDouble(),
    ((num.imaginary + 10 * mod) % mod).toDouble(),
  );
}

int calculateNumEnds(
  Map<Complex, bool> garden,
  Complex start,
  int numIterations,
  int maxSize,
) {
  final queue = <Complex, bool>{};
  queue[start] = true;

  final done = <int>[];

  for (var i = 0; i < 3 * maxSize; i++) {
    if (i % maxSize == (maxSize - 1) ~/ 2) {
      done.add(queue.length);
    }
    if (done.length == 3) {
      break;
    }

    final newQueue = <Complex, bool>{};

    for (final dir in [Complex.one, Complex(-1, 0), Complex(0, 1), Complex(0, -1)]) {
      for (final point in queue.keys) {
        if (garden.containsKey(complexMod(point + dir, maxSize))) {
          newQueue[point + dir] = true;
        }
      }
    }
    queue.clear();
    queue.addAll(newQueue);
  }

  int quadraticFunction(int n, int a, int b, int c) {
    return a + n * (b - a + ((n - 1) * (c - 2 * b + a) ~/ 2));
  }

  return quadraticFunction(numIterations ~/ maxSize, done[0], done[1], done[2]);
}