#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
    int x, y, z;
} Vec3;

typedef struct {
    Vec3 pos, vel;
} Moon;

void applyGravity(Moon *moons, int n, char axis) {
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (axis == 'x') {
                if (moons[i].pos.x > moons[j].pos.x) {
                    moons[i].vel.x--; moons[j].vel.x++;
                } else if (moons[i].pos.x < moons[j].pos.x) {
                    moons[i].vel.x++; moons[j].vel.x--;
                }
            } else if (axis == 'y') {
                if (moons[i].pos.y > moons[j].pos.y) {
                    moons[i].vel.y--; moons[j].vel.y++;
                } else if (moons[i].pos.y < moons[j].pos.y) {
                    moons[i].vel.y++; moons[j].vel.y--;
                }
            } else if (axis == 'z') {
                if (moons[i].pos.z > moons[j].pos.z) {
                    moons[i].vel.z--; moons[j].vel.z++;
                } else if (moons[i].pos.z < moons[j].pos.z) {
                    moons[i].vel.z++; moons[j].vel.z--;
                }
            }
        }
    }
}

void applyVelocity(Moon *moons, int n, char axis) {
    for (int i = 0; i < n; i++) {
        if (axis == 'x') moons[i].pos.x += moons[i].vel.x;
        else if (axis == 'y') moons[i].pos.y += moons[i].vel.y;
        else if (axis == 'z') moons[i].pos.z += moons[i].vel.z;
    }
}

int findCycle(Moon *moons, Moon *initialMoons, int n, char axis) {
    for (int steps = 1; ; steps++) {
        applyGravity(moons, n, axis);
        applyVelocity(moons, n, axis);
        int match = 1;
        for (int i = 0; i < n; i++) {
            if ((axis == 'x' && (moons[i].pos.x != initialMoons[i].pos.x || moons[i].vel.x != initialMoons[i].vel.x)) ||
                (axis == 'y' && (moons[i].pos.y != initialMoons[i].pos.y || moons[i].vel.y != initialMoons[i].vel.y)) ||
                (axis == 'z' && (moons[i].pos.z != initialMoons[i].pos.z || moons[i].vel.z != initialMoons[i].vel.z))) {
                match = 0;
                break;
            }
        }
        if (match) return steps;
    }
}

long long gcd(long long a, long long b) {
    while (b) {
        long long t = b;
        b = a % b;
        a = t;
    }
    return a;
}

long long lcm(long long a, long long b) {
    return (a / gcd(a, b)) * b;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    Moon moons[100], initialMoons[100];
    int n = 0;

    while (fscanf(file, "<x=%d, y=%d, z=%d>\n", &moons[n].pos.x, &moons[n].pos.y, &moons[n].pos.z) == 3) {
        moons[n].vel = (Vec3){0, 0, 0};
        initialMoons[n] = moons[n];
        n++;
    }
    fclose(file);

    int cycleX = findCycle(moons, initialMoons, n, 'x');
    int cycleY = findCycle(moons, initialMoons, n, 'y');
    int cycleZ = findCycle(moons, initialMoons, n, 'z');

    long long result = lcm(lcm(cycleX, cycleY), cycleZ);
    printf("%lld\n", result);
    return 0;
}