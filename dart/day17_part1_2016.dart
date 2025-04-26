import 'dart:io';
import 'dart:convert';
import 'dart:math';

void main() {
  final salt = File('input.txt').readAsStringSync().trim();
  final queue = <_State>[_State(0, 0, '')];
  while (queue.isNotEmpty) {
    final s = queue.removeAt(0);
    if (s.x == 3 && s.y == 3) {
      print(s.path);
      return;
    }
    final hash = _toHex(_md5(utf8.encode('$salt${s.path}')));
    final doors = hash.substring(0, 4).split('');
    final moves = ['U', 'D', 'L', 'R'];
    for (int i = 0; i < 4; i++) {
      if (_isOpen(doors[i])) {
        final nx = s.x + _dx[i];
        final ny = s.y + _dy[i];
        if (nx >= 0 && nx < 4 && ny >= 0 && ny < 4) {
          queue.add(_State(nx, ny, s.path + moves[i]));
        }
      }
    }
  }
}

bool _isOpen(String c) => c.codeUnitAt(0) >= 'b'.codeUnitAt(0);

const _dx = [0, 0, -1, 1];
const _dy = [-1, 1, 0, 0];

class _State {
  final int x, y;
  final String path;
  _State(this.x, this.y, this.path);
}

// --- MD5 implementation below (RFC 1321) ---

List<int> _md5(List<int> message) {
  final s = <int>[
    7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22,
    5,9,14,20, 5,9,14,20, 5,9,14,20, 5,9,14,20,
    4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23,
    6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21,
  ];
  final K = List<int>.generate(64, (i) {
    final v = (pow(2, 32) * (sin(i + 1).abs())).floor();
    return v & 0xFFFFFFFF;
  });

  final origLenBits = message.length * 8;
  final buf = <int>[...message, 0x80];
  while (buf.length % 64 != 56) buf.add(0);
  for (int i = 0; i < 8; i++) buf.add((origLenBits >> (8 * i)) & 0xFF);

  int a0 = 0x67452301, b0 = 0xEFCDAB89,
      c0 = 0x98BADCFE, d0 = 0x10325476;

  for (int i = 0; i < buf.length; i += 64) {
    final chunk = buf.sublist(i, i + 64);
    final M = List<int>.generate(16, (j) =>
      chunk[j*4] |
      (chunk[j*4+1] << 8) |
      (chunk[j*4+2] << 16) |
      (chunk[j*4+3] << 24)
    );
    int A = a0, B = b0, C = c0, D = d0;
    for (int j = 0; j < 64; j++) {
      int F, g;
      if (j < 16) {
        F = (B & C) | ((~B) & D);
        g = j;
      } else if (j < 32) {
        F = (D & B) | ((~D) & C);
        g = (5*j + 1) % 16;
      } else if (j < 48) {
        F = B ^ C ^ D;
        g = (3*j + 5) % 16;
      } else {
        F = C ^ (B | (~D));
        g = (7*j) % 16;
      }
      F = (F + A + K[j] + M[g]) & 0xFFFFFFFF;
      A = D; D = C; C = B;
      B = (B + _rotl(F, s[j])) & 0xFFFFFFFF;
    }
    a0 = (a0 + A) & 0xFFFFFFFF;
    b0 = (b0 + B) & 0xFFFFFFFF;
    c0 = (c0 + C) & 0xFFFFFFFF;
    d0 = (d0 + D) & 0xFFFFFFFF;
  }

  final out = <int>[];
  for (var v in [a0, b0, c0, d0]) {
    for (int i = 0; i < 4; i++) out.add((v >> (8 * i)) & 0xFF);
  }
  return out;
}

int _rotl(int x, int c) => ((x << c) | (x >> (32 - c))) & 0xFFFFFFFF;

String _toHex(List<int> bytes) {
  final sb = StringBuffer();
  for (var b in bytes) sb.write(b.toRadixString(16).padLeft(2, '0'));
  return sb.toString();
}
