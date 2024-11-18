
import 'dart:io';
import 'dart:math';

class RatVec3 {
  BigInt x, y, z;

  RatVec3(this.x, this.y, this.z);

  RatVec3 add(RatVec3 other) {
    return RatVec3(
      x + other.x,
      y + other.y,
      z + other.z,
    );
  }

  RatVec3 subtract(RatVec3 other) {
    return RatVec3(
      x - other.x,
      y - other.y,
      z - other.z,
    );
  }

  RatVec3 multiply(BigInt scalar) {
    return RatVec3(
      x * scalar,
      y * scalar,
      z * scalar,
    );
  }

  RatVec3 divide(BigInt scalar) {
    return RatVec3(
      x ~/ scalar,
      y ~/ scalar,
      z ~/ scalar,
    );
  }

  RatVec3 cross(RatVec3 other) {
    return RatVec3(
      y * other.z - z * other.y,
      z * other.x - x * other.z,
      x * other.y - y * other.x,
    );
  }

  BigInt dot(RatVec3 other) {
    return x * other.x + y * other.y + z * other.z;
  }
}

class HailStone {
  RatVec3 p, v;

  HailStone(this.p, this.v);

  HailStone subtract(HailStone other) {
    return HailStone(
      p.subtract(other.p),
      v.subtract(other.v),
    );
  }
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}

String solve(List<String> input) {
  List<HailStone> hailStones = readInput(input.sublist(0, 3));
  HailStone s1 = hailStones[1];
  HailStone s2 = hailStones[2];
  
  HailStone ref1 = s1.subtract(hailStones[0]);
  HailStone ref2 = s2.subtract(hailStones[0]);

  BigInt t1 = intersectionTime(ref2, ref1);
  BigInt t2 = intersectionTime(ref1, ref2);

  RatVec3 rock1 = s1.p.add(s1.v.multiply(t1));
  RatVec3 rock2 = s2.p.add(s2.v.multiply(t2));

  RatVec3 rp = rock1.subtract(
    rock2.subtract(rock1).divide(t2 - t1).multiply(t1)
  );

  return (rp.x + rp.y + rp.z).toString();
}

List<HailStone> readInput(List<String> input) {
  return input.map(readLine).toList();
}

HailStone readLine(String line) {
  RegExp regex = RegExp(r'-*\d+');
  List<String> matches = regex.allMatches(line).map((m) => m.group(0)!).toList();
  
  return HailStone(
    RatVec3(
      BigInt.parse(matches[0]),
      BigInt.parse(matches[1]),
      BigInt.parse(matches[2]),
    ),
    RatVec3(
      BigInt.parse(matches[3]),
      BigInt.parse(matches[4]),
      BigInt.parse(matches[5]),
    ),
  );
}

BigInt intersectionTime(HailStone r, HailStone s) {
  RatVec3 plane = r.p.cross(r.p.add(r.v));
  return -(s.p.dot(plane) ~/ s.v.dot(plane));
}
