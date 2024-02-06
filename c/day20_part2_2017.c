
#include <stdio.h>
#include <stdlib.h>

typedef struct Particle {
    int p[3];
    int v[3];
    int a[3];
    int alive;
} Particle;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    Particle particles[1000];
    int numParticles = 0;

    while (fscanf(file, "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>\n",
                  &particles[numParticles].p[0], &particles[numParticles].p[1], &particles[numParticles].p[2],
                  &particles[numParticles].v[0], &particles[numParticles].v[1], &particles[numParticles].v[2],
                  &particles[numParticles].a[0], &particles[numParticles].a[1], &particles[numParticles].a[2]) != EOF) {
        particles[numParticles].alive = 1;
        numParticles++;
    }

    fclose(file);

    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < numParticles; j++) {
            if (particles[j].alive) {
                particles[j].v[0] += particles[j].a[0];
                particles[j].v[1] += particles[j].a[1];
                particles[j].v[2] += particles[j].a[2];

                particles[j].p[0] += particles[j].v[0];
                particles[j].p[1] += particles[j].v[1];
                particles[j].p[2] += particles[j].v[2];
            }
        }

        for (int j = 0; j < numParticles; j++) {
            if (particles[j].alive) {
                for (int k = j + 1; k < numParticles; k++) {
                    if (particles[k].alive && particles[j].p[0] == particles[k].p[0] &&
                        particles[j].p[1] == particles[k].p[1] && particles[j].p[2] == particles[k].p[2]) {
                        particles[j].alive = 0;
                        particles[k].alive = 0;
                    }
                }
            }
        }
    }

    int count = 0;
    for (int i = 0; i < numParticles; i++) {
        if (particles[i].alive) {
            count++;
        }
    }

    printf("%d\n", count);

    return 0;
}
