import 'dart:io';
import 'dart:convert';
import 'dart:math';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final hallLine = lines[1];
  final hall = hallLine.substring(1, 12).split('');

  final top = lines[2].split('');
  final bot = lines[3].split('');
  final rooms = <String>[
    top[3], bot[3],
    top[5], bot[5],
    top[7], bot[7],
    top[9], bot[9],
  ];

  final start = hall.join() + rooms.join();
  final goal = '.' * 11 + 'AA' + 'BB' + 'CC' + 'DD';

  print(_dijkstra(start, goal));
}

const int depth = 2;
const hallStops = [0, 1, 3, 5, 7, 9, 10];
const roomHallIdx = [2, 4, 6, 8];
const energy = {'A': 1, 'B': 10, 'C': 100, 'D': 1000};

int _dijkstra(String start, String goal) {
  final best = <String, int>{start: 0};
  final queue = <_State>[_State(start, 0)];

  while (queue.isNotEmpty) {
    // find and remove lowest-cost state
    int minIdx = 0;
    for (int i = 1; i < queue.length; i++) {
      if (queue[i].cost < queue[minIdx].cost) minIdx = i;
    }
    final cur = queue.removeAt(minIdx);

    if (cur.state == goal) return cur.cost;
    if (cur.cost != best[cur.state]) continue;

    for (final e in _moves(cur.state)) {
      final nc = cur.cost + e.cost;
      if (best[e.state] == null || nc < best[e.state]!) {
        best[e.state] = nc;
        queue.add(_State(e.state, nc));
      }
    }
  }
  return -1; // unreachable
}

Iterable<_Edge> _moves(String s) sync* {
  final hall = s.substring(0, 11).split('');
  final rooms = s.substring(11).split('');

  // hallway -> room
  for (int h = 0; h < 11; h++) {
    final c = hall[h];
    if (c == '.') continue;
    final ri = 'ABCD'.indexOf(c);
    final destH = roomHallIdx[ri];
    if (!_clear(hall, h, destH)) continue;
    final ti = ri * depth, bi = ti + 1;
    if ((rooms[ti] != '.' && rooms[ti] != c) ||
        (rooms[bi] != '.' && rooms[bi] != c)) continue;
    final roomPos = rooms[bi] == '.' ? bi : ti;
    final steps = (h - destH).abs() + (roomPos % depth) + 1;
    final cost = steps * energy[c]!;
    final nh = [...hall]..[h] = '.';
    final nr = [...rooms]..[roomPos] = c;
    yield _Edge(nh.join() + nr.join(), cost);
  }

  // room -> hallway
  for (int r = 0; r < 4; r++) {
    final ti = r * depth, bi = ti + 1;
    String amph;
    int src;
    if (rooms[ti] != '.') {
      amph = rooms[ti];
      src = ti;
    } else if (rooms[bi] != '.') {
      amph = rooms[bi];
      src = bi;
    } else {
      continue;
    }
    final correct = 'ABCD'[r];
    if (amph == correct &&
        (src == bi || rooms[bi] == correct)) continue;
    final fromH = roomHallIdx[r];
    for (final dest in hallStops) {
      if (!_clear(hall, fromH, dest)) continue;
      final steps = (dest - fromH).abs() + (src % depth) + 1;
      final cost = steps * energy[amph]!;
      final nh = [...hall]..[dest] = amph;
      final nr = [...rooms]..[src] = '.';
      yield _Edge(nh.join() + nr.join(), cost);
    }
  }
}

bool _clear(List<String> hall, int a, int b) {
  final lo = a < b ? a + 1 : b;
  final hi = a < b ? b : a - 1;
  for (var i = lo; i <= hi; i++) {
    if (hall[i] != '.') return false;
  }
  return true;
}

class _State {
  final String state;
  final int cost;
  _State(this.state, this.cost);
}

class _Edge {
  final String state;
  final int cost;
  _Edge(this.state, this.cost);
}
