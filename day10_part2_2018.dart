
import 'dart:io';

class Star {
  int x;
  int y;
  int vX;
  int vY;
  Star? next;

  Star(this.x, this.y, this.vX, this.vY);
}

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var head = Star(0, 0, 0, 0);
  var tail = head;

  var re = RegExp(r'position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>');

  for (var line in lines) {
    var split = re.firstMatch(line);
    if (split == null) continue;

    var star = Star(
      int.parse(split.group(1)!),
      int.parse(split.group(2)!),
      int.parse(split.group(3)!),
      int.parse(split.group(4)!),
    );

    tail.next = star;
    tail = star;
  }

  var smallestT = 0;
  var smallestArea = 9223372036854775807;

  for (var t = 1; t < 100000; t++) {
    var maxX = 0;
    var maxY = 0;
    var minX = 0;
    var minY = 0;

    for (var temp = head.next; temp!.next != null; temp = temp.next) {
      var x = temp.x + temp.vX * t;
      if (maxX < x) {
        maxX = x;
      } else if (minX > x) {
        minX = x;
      }
      var y = temp.y + temp.vY * t;
      if (maxY < y) {
        maxY = y;
      } else if (minY > y) {
        minY = y;
      }
    }

    var lenX = maxX - minY + 1;
    var lenY = maxY - minY + 1;
    var area = lenX + lenY;

    if (smallestArea > area) {
      smallestArea = area;
      smallestT = t;
    }
  }

  print(smallestT);

  var t = smallestT;
  var maxX = 0;
  var maxY = 0;
  var minX = 0;
  var minY = 0;

  for (var temp = head.next; temp!.next != null; temp = temp.next) {
    temp.x = temp.x + temp.vX * t;
    if (maxX < temp.x) {
      maxX = temp.x;
    } else if (minX > temp.x) {
      minX = temp.x;
    }
    temp.y = temp.y + temp.vY * t;
    if (maxY < temp.y) {
      maxY = temp.y;
    } else if (minY > temp.y) {
      minY = temp.y;
    }
  }

  var mapper = List.generate(maxY - minY + 1, (_) => List.filled(maxX - minX + 1, false));

  for (var temp = head.next; temp!.next != null; temp = temp.next) {
    mapper[temp.y - minY][temp.x - minX] = true;
  }

  for (var i = 0; i < mapper.length; i++) {
    for (var j = 0; j < mapper[0].length; j++) {
      // Do something with the mapper if needed
    }
  }
}
