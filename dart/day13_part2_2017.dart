import 'dart:io';

class Scanner {
  int range;
  int position;
  int direction;

  Scanner({required this.range, this.position = 0, this.direction = 1});
}

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var firewall = <int, Scanner>{};

  for (var line in lines) {
    var fields = line.split(': ');
    var depth = int.parse(fields[0]);
    var rng = int.parse(fields[1]);
    firewall[depth] = Scanner(range: rng, position: 0, direction: 1);
  }

  var delay = 0;
  while (true) {
    if (passThrough(firewall, delay)) {
      break;
    }
    delay++;
  }

  print(delay);
}

bool passThrough(Map<int, Scanner> firewall, int delay) {
  for (var entry in firewall.entries) {
    var depth = entry.key;
    var scanner = entry.value;
    if ((depth + delay) % (2 * (scanner.range - 1)) == 0) {
      return false;
    }
  }
  return true;
}