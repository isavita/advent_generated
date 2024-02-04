import 'dart:io';

void main() {
  var file = File('input.txt');
  var moves = file.readAsLinesSync()[0].split(',');

  var programs = 'abcdefghijklmnop'.runes.toList();
  var initial = programs.join('');
  var cycleLen = 0;

  for (var i = 0; i < 1000000000; i++) {
    for (var move in moves) {
      switch (move[0]) {
        case 's':
          spin(programs, int.parse(move.substring(1)));
          break;
        case 'x':
          var positions = move.substring(1).split('/');
          exchange(programs, int.parse(positions[0]), int.parse(positions[1]));
          break;
        case 'p':
          var positions = move.substring(1).split('/');
          partner(programs, positions[0].codeUnitAt(0), positions[1].codeUnitAt(0));
          break;
      }
    }

    if (programs.join('') == initial) {
      cycleLen = i + 1;
      break;
    }
  }

  programs = 'abcdefghijklmnop'.runes.toList();

  for (var i = 0; i < 1000000000 % cycleLen; i++) {
    for (var move in moves) {
      switch (move[0]) {
        case 's':
          spin(programs, int.parse(move.substring(1)));
          break;
        case 'x':
          var positions = move.substring(1).split('/');
          exchange(programs, int.parse(positions[0]), int.parse(positions[1]));
          break;
        case 'p':
          var positions = move.substring(1).split('/');
          partner(programs, positions[0].codeUnitAt(0), positions[1].codeUnitAt(0));
          break;
      }
    }
  }

  print(String.fromCharCodes(programs));
}

void spin(List<int> programs, int x) {
  var n = programs.length;
  var temp = List<int>.from(programs);

  for (var i = 0; i < n; i++) {
    programs[(i + x) % n] = temp[i];
  }
}

void exchange(List<int> programs, int A, int B) {
  var temp = programs[A];
  programs[A] = programs[B];
  programs[B] = temp;
}

void partner(List<int> programs, int A, int B) {
  var indexA = programs.indexOf(A);
  var indexB = programs.indexOf(B);
  exchange(programs, indexA, indexB);
}