import 'dart:io';
import 'dart:convert';
import 'dart:math';

void main() {
  // Read and insert the unfolded diagram rows
  var lines = File('input.txt').readAsLinesSync();
  // Insert between the two original room rows
  lines.insert(3, '  #D#C#B#A#');
  lines.insert(4, '  #D#B#A#C#');

  // Parse the hallway (cols 1–11)
  final hall = lines[1].substring(1, 12).split('');

  // Build the 4×4 rooms from rows 2–5
  final roomRows = List.generate(4, (i) => lines[2 + i].split(''));
  final rooms = <String>[];
  for (int r = 0; r < 4; r++) {
    for (int d = 0; d < 4; d++) {
      rooms.add(roomRows[d][3 + r * 2]);
    }
  }

  final start = hall.join() + rooms.join();              // 11 + 16 = 27 chars
  final goal  = '.' * 11 + 'AAAA' + 'BBBB' + 'CCCC' + 'DDDD';

  print(_dijkstra(start, goal));
}

const int depth = 4;
const hallStops   = [0, 1, 3, 5, 7, 9, 10];
const roomHallIdx = [2, 4, 6, 8];
const energy      = {'A': 1, 'B': 10, 'C': 100, 'D': 1000};

int _dijkstra(String start, String goal) {
  final best  = <String, int>{ start: 0 };
  final queue = [_State(start, 0)];

  while (queue.isNotEmpty) {
    // extract minimal‐cost state
    int mi = 0;
    for (int i = 1; i < queue.length; i++) {
      if (queue[i].cost < queue[mi].cost) mi = i;
    }
    final cur = queue.removeAt(mi);
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
  return -1; // unreachable if solvable
}

Iterable<_Edge> _moves(String s) sync* {
  final hall  = s.substring(0, 11).split('');
  final rooms = s.substring(11).split('');

  // --- hallway → room ---
  for (int h = 0; h < 11; h++) {
    final c = hall[h];
    if (c == '.') continue;
    final ri    = 'ABCD'.indexOf(c);
    final destH = roomHallIdx[ri];
    // must be clear (including destH) and destH empty
    if (!_clear(hall, h, destH) || hall[destH] != '.') continue;

    final startIdx = ri * depth;
    // room must contain only . or c
    bool ok = true;
    for (int i = 0; i < depth; i++) {
      final o = rooms[startIdx + i];
      if (o != '.' && o != c) { ok = false; break; }
    }
    if (!ok) continue;
    // choose deepest empty slot
    int slot = startIdx + depth - 1;
    for (int i = depth - 1; i >= 0; i--) {
      if (rooms[startIdx + i] == '.') {
        slot = startIdx + i;
        break;
      }
    }
    final steps = (h - destH).abs() + (slot - startIdx) + 1;
    final cost  = steps * energy[c]!;

    final nh = [...hall]..[h] = '.';
    final nr = [...rooms]..[slot] = c;
    yield _Edge(nh.join() + nr.join(), cost);
  }

  // --- room → hallway ---
  for (int r = 0; r < 4; r++) {
    final startIdx = r * depth;
    int src = -1;
    String amph = '';
    // find topmost occupant
    for (int i = 0; i < depth; i++) {
      if (rooms[startIdx + i] != '.') {
        src  = startIdx + i;
        amph = rooms[src];
        break;
      }
    }
    if (src < 0) continue;
    // skip if already in correct room and all below match
    final correct = 'ABCD'[r];
    final depthIdx = src - startIdx;
    bool allOk = true;
    for (int i = depthIdx; i < depth; i++) {
      if (rooms[startIdx + i] != correct) {
        allOk = false;
        break;
      }
    }
    if (allOk) continue;

    final fromH = roomHallIdx[r];
    for (final dst in hallStops) {
      if (!_clear(hall, fromH, dst) || hall[dst] != '.') continue;
      final steps = (dst - fromH).abs() + depthIdx + 1;
      final cost  = steps * energy[amph]!;

      final nh = [...hall]..[dst] = amph;
      final nr = [...rooms]..[src] = '.';
      yield _Edge(nh.join() + nr.join(), cost);
    }
  }
}

// inclusive between a and b
bool _clear(List<String> hall, int a, int b) {
  final lo = a < b ? a + 1 : b;
  final hi = a < b ? b     : a - 1;
  for (int i = lo; i <= hi; i++) {
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
