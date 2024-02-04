
import 'dart:io';

void main() {
  List<String> danceMoves = File('input.txt').readAsStringSync().trim().split(',');

  List<String> programs = List.generate(16, (index) => String.fromCharCode(97 + index));

  for (String move in danceMoves) {
    if (move[0] == 's') {
      int size = int.parse(move.substring(1));
      programs = [...programs.sublist(16 - size), ...programs.sublist(0, 16 - size)];
    } else if (move[0] == 'x') {
      List<int> positions = move.substring(1).split('/').map(int.parse).toList();
      String temp = programs[positions[0]];
      programs[positions[0]] = programs[positions[1]];
      programs[positions[1]] = temp;
    } else if (move[0] == 'p') {
      List<String> partners = move.substring(1).split('/');
      int indexA = programs.indexOf(partners[0]);
      int indexB = programs.indexOf(partners[1]);
      String temp = programs[indexA];
      programs[indexA] = programs[indexB];
      programs[indexB] = temp;
    }
  }

  print(programs.join());
}
