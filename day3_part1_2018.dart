import 'dart:io';

class Claim {
  int id;
  int left;
  int top;
  int width;
  int height;

  Claim(this.id, this.left, this.top, this.width, this.height);

  factory Claim.fromString(String s) {
    List<String> parts = s.split(' ');
    int id = int.parse(parts[0].substring(1));
    List<String> coords = parts[2].split(',');
    int left = int.parse(coords[0]);
    int top = int.parse(coords[1].substring(0, coords[1].length - 1));
    List<String> dimensions = parts[3].split('x');
    int width = int.parse(dimensions[0]);
    int height = int.parse(dimensions[1]);
    return Claim(id, left, top, width, height);
  }
}

List<Claim> readClaims(String filename) {
  File file = File(filename);
  List<Claim> claims = [];
  List<String> lines = file.readAsLinesSync();
  for (String line in lines) {
    claims.add(Claim.fromString(line));
  }
  return claims;
}

int countOverlappingInches(List<Claim> claims) {
  Map<String, int> fabric = {};
  for (Claim claim in claims) {
    for (int i = claim.left; i < claim.left + claim.width; i++) {
      for (int j = claim.top; j < claim.top + claim.height; j++) {
        String coord = '$i,$j';
        fabric[coord] = (fabric[coord] ?? 0) + 1;
      }
    }
  }

  int overlapping = 0;
  fabric.values.forEach((count) {
    if (count > 1) {
      overlapping++;
    }
  });

  return overlapping;
}

void main() {
  List<Claim> claims = readClaims('input.txt');
  int overlapping = countOverlappingInches(claims);
  print(overlapping);
}