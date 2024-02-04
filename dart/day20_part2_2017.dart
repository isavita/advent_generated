import 'dart:io';

class Particle {
  List<int> p = [0, 0, 0];
  List<int> v = [0, 0, 0];
  List<int> a = [0, 0, 0];
}

void main() {
  var file = File('input.txt');
  var particles = <Particle>[];

  var lines = file.readAsLinesSync();
  for (var line in lines) {
    var parts = line.split(', ');

    var p = Particle();
    for (var i = 0; i < parts.length; i++) {
      var coords = parts[i].substring(3, parts[i].length - 1).split(',');
      for (var j = 0; j < coords.length; j++) {
        var num = int.parse(coords[j]);
        switch (i) {
          case 0:
            p.p[j] = num;
            break;
          case 1:
            p.v[j] = num;
            break;
          case 2:
            p.a[j] = num;
            break;
        }
      }
    }
    particles.add(p);
  }

  for (var tick = 0; tick < 1000; tick++) {
    var positions = <String, int>{};
    for (var i = 0; i < particles.length; i++) {
      for (var j = 0; j < 3; j++) {
        particles[i].v[j] += particles[i].a[j];
        particles[i].p[j] += particles[i].v[j];
      }
      var posStr = '${particles[i].p[0]},${particles[i].p[1]},${particles[i].p[2]}';
      positions[posStr] = (positions[posStr] ?? 0) + 1;
    }

    var newParticles = <Particle>[];
    for (var particle in particles) {
      var posStr = '${particle.p[0]},${particle.p[1]},${particle.p[2]}';
      if (positions[posStr] == 1) {
        newParticles.add(particle);
      }
    }
    particles = List.from(newParticles);
  }

  print(particles.length);
}