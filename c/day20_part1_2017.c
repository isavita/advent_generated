
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct {
    int x, y, z;
} Vec3;

typedef struct {
    Vec3 p, v, a;
} Particle;

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    int num_particles = 1000; // Assuming 1000 particles
    Particle particles[num_particles];

    for (int i = 0; i < num_particles; i++) {
        fscanf(fp, "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>\n",
            &particles[i].p.x, &particles[i].p.y, &particles[i].p.z,
            &particles[i].v.x, &particles[i].v.y, &particles[i].v.z,
            &particles[i].a.x, &particles[i].a.y, &particles[i].a.z);
    }

    fclose(fp);

    int closest_particle = 0;
    double min_distance = 1e9; // Set to a large initial value

    for (int i = 0; i < num_particles; i++) {
        double distance = abs(particles[i].a.x) + abs(particles[i].a.y) + abs(particles[i].a.z);
        if (distance < min_distance) {
            min_distance = distance;
            closest_particle = i;
        }
    }

    printf("%d\n", closest_particle);

    return 0;
}
