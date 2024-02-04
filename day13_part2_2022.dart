import 'dart:io';
import 'dart:convert';

void main() {
  var file = File('input.txt');
  var contents = file.readAsStringSync();
  var packets = <dynamic>[];
  for (var pair in contents.split('\n\n')) {
    var sp = pair.split('\n');
    var first = jsonDecode(sp[0]);
    var second = jsonDecode(sp[1]);
    packets.addAll([first, second]);
  }

  var divider1 = jsonDecode('[[2]]');
  var divider2 = jsonDecode('[[6]]');
  packets.addAll([divider1, divider2]);
  packets.sort((a, b) => compare(a, b));
  var divider1Pos = packets.indexWhere((packet) => compare(packet, divider1) >= 0);
  var divider2Pos = packets.indexWhere((packet) => compare(packet, divider2) >= 0);
  print((divider1Pos + 1) * (divider2Pos + 1));
}

int compare(dynamic a, dynamic b) {
  if (a is num && b is num) {
    return (a as int).compareTo(b as int);
  } else if (a is num) {
    return compare([a], b);
  } else if (b is num) {
    return compare(a, [b]);
  } else {
    List<dynamic> aa = a;
    List<dynamic> bb = b;
    for (var i = 0; i < aa.length && i < bb.length; i++) {
      var c = compare(aa[i], bb[i]);
      if (c != 0) {
        return c;
      }
    }
    return aa.length.compareTo(bb.length);
  }
}