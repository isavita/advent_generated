import 'dart:io';
import 'dart:math';

class Particle {
  List<int> p, v, a;

  Particle(this.p, this.v, this.a);
}

int abs(int x) => x < 0 ? -x : x;

int manhattan(List<int> x) => abs(x[0]) + abs(x[1]) + abs(x[2]);

void main() {
  File('input.txt')
      .readAsLines()
      .then((lines) {
    List<Particle> particles = [];
    for (String line in lines) {
      List<String> parts = line.split(', ');
      List<int> p = parts[0].substring(3, parts[0].length - 1).split(',').map(int.parse).toList();
      List<int> v = parts[1].substring(3, parts[1].length - 1).split(',').map(int.parse).toList();
      List<int> a = parts[2].substring(3, parts[2].length - 1).split(',').map(int.parse).toList();
      particles.add(Particle(p, v, a));
    }

    int closestParticle = 0;
    int minAccel = double.maxFinite.toInt();
    int minVelocity = double.maxFinite.toInt();
    int minPosition = double.maxFinite.toInt();

    for (int i = 0; i < particles.length; i++) {
      Particle particle = particles[i];
      int accel = manhattan(particle.a);
      int velocity = manhattan(particle.v);
      int position = manhattan(particle.p);

      if (accel < minAccel || (accel == minAccel && velocity < minVelocity) || (accel == minAccel && velocity == minVelocity && position < minPosition)) {
        minAccel = accel;
        minVelocity = velocity;
        minPosition = position;
        closestParticle = i;
      }
    }

    print(closestParticle);
  });
}