
import 'dart:convert';
import 'dart:io';

void main() {
  var packets = [];
  var sum = 0;
  var input = File('input.txt').readAsStringSync();
  var pairs = input.split('\n\n');
  
  for (var i = 0; i < pairs.length; i++) {
    var sp = pairs[i].split('\n');
    var first = jsonDecode(sp[0]);
    var second = jsonDecode(sp[1]);
    
    packets.addAll([first, second]);
    
    if (compare(first, second) == -1) {
      sum += i + 1;
    }
  }
  
  print(sum);
}

int compare(dynamic a, dynamic b) {
  if (a is num && b is num) {
    return (a - b).sign.toInt();
  } else if (a is num) {
    return compare([a], b);
  } else if (b is num) {
    return compare(a, [b]);
  } else {
    List aa = a;
    List bb = b;
    for (var i = 0; i < aa.length && i < bb.length; i++) {
      var c = compare(aa[i], bb[i]);
      if (c != 0) {
        return c;
      }
    }
    return (aa.length - bb.length).sign.toInt();
  }
}
