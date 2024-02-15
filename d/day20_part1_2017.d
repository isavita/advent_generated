
import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.math;

struct Particle {
    int[3] p;
    int[3] v;
    int[3] a;
}

int abs(int x) {
    return x < 0 ? -x : x;
}

int manhattan(int[3] x) {
    return abs(x[0]) + abs(x[1]) + abs(x[2]);
}

void main() {
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;
    file.close();

    Particle[] particles;
    foreach (line; lines) {
        auto parts = line.split(", ");

        Particle p;
        foreach (i, part; parts) {
            auto coords = part[3 .. $-1].split(",");
            foreach (j, coord; coords) {
                auto num = to!int(coord);
                final switch (i) {
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
        particles ~= p;
    }

    int closestParticle;
    int minAccel = int.max;
    int minVelocity = int.max;
    int minPosition = int.max;

    foreach (i, particle; particles) {
        int accel = manhattan(particle.a);
        int velocity = manhattan(particle.v);
        int position = manhattan(particle.p);

        if (accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
            (accel == minAccel && velocity == minVelocity && position < minPosition)) {
            minAccel = accel;
            minVelocity = velocity;
            minPosition = position;
            closestParticle = cast(int)i;
        }
    }

    writeln(closestParticle);
}
