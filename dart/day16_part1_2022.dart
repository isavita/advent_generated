
import 'dart:io';

class Valve {
  String id;
  int flow;
  Map<String, int> tunnels = {};

  Valve(this.id, this.flow);
}

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final valves = <String, Valve>{};

  input.split('\n').forEach((line) {
    final parts = line.split('; ');
    final valveInfo = parts[0].split(' ');
    final tunnelInfo = parts[1].split(' ');
    final id = valveInfo[1];
    final flow = int.parse(valveInfo[4].split('=')[1]);
    final tunnels = tunnelInfo.sublist(4).map((t) => t.replaceAll(',', '')).toList();
    valves[id] = Valve(id, flow)..tunnels.addAll({for (var t in tunnels) t: 1});
  });

  for (var k in valves.keys) {
    for (var i in valves.keys) {
      for (var j in valves.keys) {
        final dik = valves[i]!.tunnels[k];
        final dkj = valves[k]!.tunnels[j];
        if (dik != null && dkj != null) {
          final dij = valves[i]!.tunnels[j];
          if (dij == null || dij > dik + dkj) {
            valves[i]!.tunnels[j] = dik + dkj;
          }
        }
      }
    }
  }

  final open = valves.values.where((v) => v.flow > 0).map((v) => v.id).toList();
  print(maxPressure(valves, 'AA', 30, 0, open, 0));
}

int maxPressure(Map<String, Valve> valves, String curr, int minute, int pressure, List<String> open, int d) {
  int max = pressure;
  for (var next in open) {
    final newOpen = open.where((v) => v != next).toList();
    final timeLeft = minute - valves[curr]!.tunnels[next]! - 1;
    if (timeLeft > 0) {
      max = max.max(maxPressure(valves, next, timeLeft, timeLeft * valves[next]!.flow + pressure, newOpen, d + 1));
    }
  }
  return max;
}

extension on int {
  int max(int other) => this > other ? this : other;
}
