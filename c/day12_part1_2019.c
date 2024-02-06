
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x, y, z;
} Vec3;

typedef struct {
    Vec3 pos, vel;
} Moon;

int Abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

void applyGravity(Moon moons[], int numMoons) {
    for (int i = 0; i < numMoons; i++) {
        for (int j = i + 1; j < numMoons; j++) {
            if (moons[i].pos.x > moons[j].pos.x) {
                moons[i].vel.x--;
                moons[j].vel.x++;
            } else if (moons[i].pos.x < moons[j].pos.x) {
                moons[i].vel.x++;
                moons[j].vel.x--;
            }

            if (moons[i].pos.y > moons[j].pos.y) {
                moons[i].vel.y--;
                moons[j].vel.y++;
            } else if (moons[i].pos.y < moons[j].pos.y) {
                moons[i].vel.y++;
                moons[j].vel.y--;
            }

            if (moons[i].pos.z > moons[j].pos.z) {
                moons[i].vel.z--;
                moons[j].vel.z++;
            } else if (moons[i].pos.z < moons[j].pos.z) {
                moons[i].vel.z++;
                moons[j].vel.z--;
            }
        }
    }
}

void applyVelocity(Moon moons[], int numMoons) {
    for (int i = 0; i < numMoons; i++) {
        moons[i].pos.x += moons[i].vel.x;
        moons[i].pos.y += moons[i].vel.y;
        moons[i].pos.z += moons[i].vel.z;
    }
}

int totalEnergy(Moon moons[], int numMoons) {
    int total = 0;
    for (int i = 0; i < numMoons; i++) {
        int pot = Abs(moons[i].pos.x) + Abs(moons[i].pos.y) + Abs(moons[i].pos.z);
        int kin = Abs(moons[i].vel.x) + Abs(moons[i].vel.y) + Abs(moons[i].vel.z);
        total += pot * kin;
    }
    return total;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int numMoons = 0;
    while (!feof(file)) {
        int x, y, z;
        fscanf(file, "<x=%d, y=%d, z=%d>\n", &x, &y, &z);
        numMoons++;
    }

    Moon moons[numMoons];
    rewind(file);

    for (int i = 0; i < numMoons; i++) {
        int x, y, z;
        fscanf(file, "<x=%d, y=%d, z=%d>\n", &x, &y, &z);
        moons[i].pos.x = x;
        moons[i].pos.y = y;
        moons[i].pos.z = z;
        moons[i].vel.x = 0;
        moons[i].vel.y = 0;
        moons[i].vel.z = 0;
    }

    fclose(file);

    for (int step = 0; step < 1000; step++) {
        applyGravity(moons, numMoons);
        applyVelocity(moons, numMoons);
    }

    printf("%d\n", totalEnergy(moons, numMoons));

    return 0;
}
