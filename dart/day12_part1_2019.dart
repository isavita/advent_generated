
import 'dart:io';

class Moon {
  int x, y, z;
  int velX = 0, velY = 0, velZ = 0;

  Moon(this.x, this.y, this.z);
}

void applyGravity(List<Moon> moons) {
  for (int i = 0; i < moons.length; i++) {
    for (int j = i + 1; j < moons.length; j++) {
      if (moons[i].x < moons[j].x) {
        moons[i].velX++;
        moons[j].velX--;
      } else if (moons[i].x > moons[j].x) {
        moons[i].velX--;
        moons[j].velX++;
      }

      if (moons[i].y < moons[j].y) {
        moons[i].velY++;
        moons[j].velY--;
      } else if (moons[i].y > moons[j].y) {
        moons[i].velY--;
        moons[j].velY++;
      }

      if (moons[i].z < moons[j].z) {
        moons[i].velZ++;
        moons[j].velZ--;
      } else if (moons[i].z > moons[j].z) {
        moons[i].velZ--;
        moons[j].velZ++;
      }
    }
  }
}

void applyVelocity(List<Moon> moons) {
  for (var moon in moons) {
    moon.x += moon.velX;
    moon.y += moon.velY;
    moon.z += moon.velZ;
  }
}

int calculateEnergy(List<Moon> moons) {
  int totalEnergy = 0;
  for (var moon in moons) {
    int potentialEnergy = moon.x.abs() + moon.y.abs() + moon.z.abs();
    int kineticEnergy = moon.velX.abs() + moon.velY.abs() + moon.velZ.abs();
    totalEnergy += potentialEnergy * kineticEnergy;
  }
  return totalEnergy;
}

void main() {
  List<Moon> moons = [];
  var lines = File('input.txt').readAsLinesSync();
  for (var line in lines) {
    var values = line.substring(1, line.length - 1).split(', ');
    var x = int.parse(values[0].substring(2));
    var y = int.parse(values[1].substring(2));
    var z = int.parse(values[2].substring(2));
    moons.add(Moon(x, y, z));
  }

  for (int step = 0; step < 1000; step++) {
    applyGravity(moons);
    applyVelocity(moons);
  }

  int totalEnergy = calculateEnergy(moons);
  print(totalEnergy);
}
