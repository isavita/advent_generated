
import 'dart:io';

class Vec3 {
  int x, y, z;

  Vec3(this.x, this.y, this.z);
}

class Moon {
  Vec3 pos, vel;

  Moon(this.pos, this.vel);
}

void applyGravity(List<Moon> moons, String axis) {
  for (int i = 0; i < moons.length; i++) {
    for (int j = i + 1; j < moons.length; j++) {
      switch (axis) {
        case "x":
          if (moons[i].pos.x > moons[j].pos.x) {
            moons[i].vel.x--;
            moons[j].vel.x++;
          } else if (moons[i].pos.x < moons[j].pos.x) {
            moons[i].vel.x++;
            moons[j].vel.x--;
          }
          break;
        case "y":
          if (moons[i].pos.y > moons[j].pos.y) {
            moons[i].vel.y--;
            moons[j].vel.y++;
          } else if (moons[i].pos.y < moons[j].pos.y) {
            moons[i].vel.y++;
            moons[j].vel.y--;
          }
          break;
        case "z":
          if (moons[i].pos.z > moons[j].pos.z) {
            moons[i].vel.z--;
            moons[j].vel.z++;
          } else if (moons[i].pos.z < moons[j].pos.z) {
            moons[i].vel.z++;
            moons[j].vel.z--;
          }
          break;
      }
    }
  }
}

void applyVelocity(List<Moon> moons, String axis) {
  for (int i = 0; i < moons.length; i++) {
    switch (axis) {
      case "x":
        moons[i].pos.x += moons[i].vel.x;
        break;
      case "y":
        moons[i].pos.y += moons[i].vel.y;
        break;
      case "z":
        moons[i].pos.z += moons[i].vel.z;
        break;
    }
  }
}

int findCycle(List<Moon> moons, List<Moon> initialMoons, String axis) {
  int steps = 1;
  while (true) {
    applyGravity(moons, axis);
    applyVelocity(moons, axis);

    bool match = true;
    for (int i = 0; i < moons.length; i++) {
      switch (axis) {
        case "x":
          if (moons[i].pos.x != initialMoons[i].pos.x || moons[i].vel.x != initialMoons[i].vel.x) {
            match = false;
          }
          break;
        case "y":
          if (moons[i].pos.y != initialMoons[i].pos.y || moons[i].vel.y != initialMoons[i].vel.y) {
            match = false;
          }
          break;
        case "z":
          if (moons[i].pos.z != initialMoons[i].pos.z || moons[i].vel.z != initialMoons[i].vel.z) {
            match = false;
          }
          break;
      }
    }

    if (match) {
      return steps;
    }

    steps++;
  }
}

BigInt lcm(int a, int b) {
  BigInt bigA = BigInt.from(a);
  BigInt bigB = BigInt.from(b);
  return (bigA * bigB) ~/ (bigA.gcd(bigB));
}

void main() {
  File file = File("input.txt");
  List<Moon> moons = [];
  List<Moon> initialMoons = [];

  file.readAsLines().then((List<String> lines) {
    for (String line in lines) {
      RegExp regExp = RegExp(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>");
      RegExpMatch match = regExp.firstMatch(line)!;
      int x = int.parse(match.group(1)!);
      int y = int.parse(match.group(2)!);
      int z = int.parse(match.group(3)!);
      moons.add(Moon(Vec3(x, y, z), Vec3(0, 0, 0)));
      initialMoons.add(Moon(Vec3(x, y, z), Vec3(0, 0, 0)));
    }

    int cycleX = findCycle(moons, initialMoons, "x");
    int cycleY = findCycle(moons, initialMoons, "y");
    int cycleZ = findCycle(moons, initialMoons, "z");

    BigInt lcmXY = lcm(cycleX, cycleY);
    BigInt lcmXYZ = lcm(int.parse(lcmXY.toString()), cycleZ);

    print(lcmXYZ);
  });
}
