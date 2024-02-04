
import 'dart:io';

void main() {
  var file = new File('input.txt');
  var lines = file.readAsLinesSync();
  
  for (var i = 0; i < lines.length - 1; i++) {
    for (var j = i + 1; j < lines.length; j++) {
      var diff = 0;
      for (var k = 0; k < lines[i].length; k++) {
        if (lines[i][k] != lines[j][k]) {
          diff++;
          if (diff > 1) {
            break;
          }
        }
      }
      if (diff == 1) {
        var common = "";
        for (var k = 0; k < lines[i].length; k++) {
          if (lines[i][k] == lines[j][k]) {
            common += lines[i][k];
          }
        }
        print(common);
        return;
      }
    }
  }
}
