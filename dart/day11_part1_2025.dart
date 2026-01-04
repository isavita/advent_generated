
import 'dart:io';

const _start = 'you';
const _end = 'out';

void main() {
  final adj = <String, List<String>>{};
  final memo = <String, int>{};

  for (final l in File('input.txt').readAsLinesSync()) {
    final parts = l.split(':');
    if (parts.isEmpty) continue;
    adj[parts[0].trim()] =
        parts.length > 1 ? parts[1].trim().split(RegExp(r'\s+')) : [];
  }

  int dfs(String u) {
    if (u == _end) return 1;
    if (memo.containsKey(u)) return memo[u]!;
    int cnt = 0;
    for (final v in adj[u] ?? const []) cnt += dfs(v);
    return memo[u] = cnt;
  }

  print(dfs(_start));
}
