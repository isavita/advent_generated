import 'dart:io';
import 'dart:convert';
import 'dart:math';

void main() {
  final secret = File('input.txt').readAsStringSync().trim();
  int number = 1;
  while (true) {
    final bytes = utf8.encode('$secret$number');
    final hashBytes = _md5(bytes);
    final hashHex = _toHex(hashBytes);
    if (hashHex.startsWith('00000')) {
      print(number);
      break;
    }
    number++;
  }
}

List<int> _md5(List<int> message) {
  // Constants per RFC 1321
  final s = <int>[
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
  ];

  final K = List<int>.generate(64, (i) {
    final v = (pow(2, 32) * (sin(i + 1).abs())).floor();
    return v & 0xFFFFFFFF;
  });

  // Pre-processing: padding
  final originalLength = message.length * 8;
  final padded = List<int>.from(message);
  padded.add(0x80);
  while ((padded.length % 64) != 56) {
    padded.add(0);
  }
  // Append original length in bits, little-endian 64-bit
  for (int i = 0; i < 8; i++) {
    padded.add((originalLength >> (8 * i)) & 0xFF);
  }

  // Initial hash values
  int a0 = 0x67452301;
  int b0 = 0xEFCDAB89;
  int c0 = 0x98BADCFE;
  int d0 = 0x10325476;

  // Process each 512-bit chunk
  for (int i = 0; i < padded.length; i += 64) {
    final chunk = padded.sublist(i, i + 64);
    final M = List<int>.generate(16, (j) {
      return chunk[j * 4] |
          (chunk[j * 4 + 1] << 8) |
          (chunk[j * 4 + 2] << 16) |
          (chunk[j * 4 + 3] << 24);
    });

    int A = a0;
    int B = b0;
    int C = c0;
    int D = d0;

    for (int j = 0; j < 64; j++) {
      int F, g;
      if (j < 16) {
        F = (B & C) | ((~B) & D);
        g = j;
      } else if (j < 32) {
        F = (D & B) | ((~D) & C);
        g = (5 * j + 1) % 16;
      } else if (j < 48) {
        F = B ^ C ^ D;
        g = (3 * j + 5) % 16;
      } else {
        F = C ^ (B | (~D));
        g = (7 * j) % 16;
      }
      F = (F + A + K[j] + M[g]) & 0xFFFFFFFF;
      A = D;
      D = C;
      C = B;
      B = (B + _leftRotate(F, s[j])) & 0xFFFFFFFF;
    }

    a0 = (a0 + A) & 0xFFFFFFFF;
    b0 = (b0 + B) & 0xFFFFFFFF;
    c0 = (c0 + C) & 0xFFFFFFFF;
    d0 = (d0 + D) & 0xFFFFFFFF;
  }

  // Output is little-endian of a0,b0,c0,d0
  final output = <int>[];
  for (var v in [a0, b0, c0, d0]) {
    for (int i = 0; i < 4; i++) {
      output.add((v >> (8 * i)) & 0xFF);
    }
  }
  return output;
}

int _leftRotate(int x, int c) {
  return ((x << c) | (x >> (32 - c))) & 0xFFFFFFFF;
}

String _toHex(List<int> bytes) {
  final buffer = StringBuffer();
  for (var b in bytes) {
    buffer.write(b.toRadixString(16).padLeft(2, '0'));
  }
  return buffer.toString();
}
