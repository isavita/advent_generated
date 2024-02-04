import 'dart:io';

void main() {
  var root = [''];
  var dirs = {};
  var files = {};
  var curr = [];
  var sortedSizes = [];

  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  for (var line in lines) {
    var txt = line.split(' ');
    if (txt[0] == '\$') {
      if (txt[1] == 'cd') {
        if (txt[2] == '/') {
          curr = root;
        } else if (txt[2] == '..') {
          curr.removeLast();
        } else {
          curr.add(txt[2]);
        }
        dirs[curr.join('/')] = 0;
      }
    } else {
      if (txt[0] != 'dir') {
        files[curr.join('/') + '/' + txt[1]] = int.parse(txt[0]);
      }
    }
  }

  files.forEach((f, s) {
    var path = f.split('/');
    for (var i = 1; i < path.length; i++) {
      dirs[path.sublist(0, i).join('/')] = (dirs[path.sublist(0, i).join('/')] ?? 0) + s;
    }
  });

  dirs.forEach((k, v) {
    sortedSizes.add(v);
  });

  sortedSizes.sort();
  var total = 70000000;
  var want = 30000000;
  var available = total - dirs[''];
  print(sortedSizes[sortedSizes.indexWhere((element) => element >= want - available)]);
}