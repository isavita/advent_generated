import 'dart:io';

void main() {
  var file = new File('input.txt');
  var lines = file.readAsLinesSync();

  List<String> replacements = [];
  String molecule = '';

  for (var line in lines) {
    if (line.isEmpty) {
      continue;
    }
    if (line.contains(' => ')) {
      replacements.add(line);
    } else {
      molecule = line;
    }
  }

  var molecules = <String, bool>{};
  for (var replacement in replacements) {
    var parts = replacement.split(' => ');
    for (var i = 0; i < molecule.length; i++) {
      if (molecule.substring(i).startsWith(parts[0])) {
        var newMolecule = molecule.substring(0, i) + parts[1] + molecule.substring(i + parts[0].length);
        molecules[newMolecule] = true;
      }
    }
  }

  print(molecules.length);
}