
/**
 * Advent of Code 2023, Day 24 - Never Tell Me The Odds
 *
 * Author: Your Name
 *
 * This program solves both parts of the Day 24 challenge.
 *
 * Part 1: Calculates the number of future intersections of hailstone paths in a 2D plane
 *         within a specified test area. It solves this by treating each hailstone's path
 *         as a line and finding the intersection point for each pair of lines using
 *         Cramer's rule.
 *
 * Part 2: Finds the initial position of a rock that would collide with every hailstone.
 *         This is modeled as a system of linear equations derived from the physical
 *         constraint that the relative position and relative velocity vectors between
 *         the rock and any hailstone must be collinear at the moment of collision.
 *         A 6x6 system is formed using data from three hailstones and solved using
 *         Gaussian elimination to find the rock's initial position and velocity.
 *
 * The program reads its input from a file named "input.txt".
 *
 * To compile and run:
 * gcc -o day24 day24.c -lm -O3
 * ./day24
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_HAILSTONES 500

typedef struct {
    long long px, py, pz;
    long long vx, vy, vz;
} Hailstone;

static Hailstone hailstones[MAX_HAILSTONES];
static int num_hailstones = 0;

/**
 * Parses the input file and populates the global hailstones array.
 */
void parse_input(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Error opening input.txt");
        exit(EXIT_FAILURE);
    }

    char line[256];
    while (fgets(line, sizeof(line), file) && num_hailstones < MAX_HAILSTONES) {
        if (sscanf(line, "%lld, %lld, %lld @ %lld, %lld, %lld",
                   &hailstones[num_hailstones].px, &hailstones[num_hailstones].py, &hailstones[num_hailstones].pz,
                   &hailstones[num_hailstones].vx, &hailstones[num_hailstones].vy, &hailstones[num_hailstones].vz) == 6) {
            num_hailstones++;
        }
    }
    fclose(file);
}

/**
 * Solves Part 1: Counts 2D path intersections within a test area.
 */
void solve_part1() {
    long long intersection_count = 0;
    const double min_coord = 200000000000000.0;
    const double max_coord = 400000000000000.0;
    
    for (int i = 0; i < num_hailstones; i++) {
        for (int j = i + 1; j < num_hailstones; j++) {
            Hailstone h1 = hailstones[i];
            Hailstone h2 = hailstones[j];

            long long det = h1.vx * h2.vy - h1.vy * h2.vx;
            if (det == 0) continue; // Parallel paths

            // Using long double for precision with large coordinates
            long double t1 = (long double)((h2.px - h1.px) * h2.vy - (h2.py - h1.py) * h2.vx) / det;
            long double t2 = (long double)((h2.px - h1.px) * h1.vy - (h2.py - h1.py) * h1.vx) / det;

            if (t1 > 0 && t2 > 0) { // Check if intersection is in the future
                long double ix = (long double)h1.px + h1.vx * t1;
                long double iy = (long double)h1.py + h1.vy * t1;

                if (ix >= min_coord && ix <= max_coord && iy >= min_coord && iy <= max_coord) {
                    intersection_count++;
                }
            }
        }
    }
    printf("Part 1: %lld\n", intersection_count);
}

/**
 * Solves a 6x6 system of linear equations A*x = b using Gaussian elimination.
 * The augmented matrix [A|b] is passed and the solution vector x overwrites b.
 */
void solve_linear_system(long double A[6][7]) {
    for (int i = 0; i < 6; i++) {
        // Partial pivoting for numerical stability
        int max_row = i;
        for (int k = i + 1; k < 6; k++) {
            if (fabsl(A[k][i]) > fabsl(A[max_row][i])) max_row = k;
        }
        for (int k = i; k < 7; k++) {
            long double temp = A[i][k];
            A[i][k] = A[max_row][k];
            A[max_row][k] = temp;
        }

        // Forward elimination
        for (int k = i + 1; k < 6; k++) {
            long double factor = A[k][i] / A[i][i];
            for (int j = i; j < 7; j++) {
                A[k][j] -= factor * A[i][j];
            }
        }
    }

    // Back substitution
    for (int i = 5; i >= 0; i--) {
        for (int j = i + 1; j < 6; j++) {
            A[i][6] -= A[i][j] * A[j][6];
        }
        A[i][6] /= A[i][i];
    }
}

/**
 * Solves Part 2: Finds the initial position of a rock to hit all hailstones.
 */
void solve_part2() {
    if (num_hailstones < 3) {
        printf("Part 2: Not enough data points.\n");
        return;
    }

    // Use first three hailstones, assuming they are not pathologically aligned.
    Hailstone h0 = hailstones[0], h1 = hailstones[1], h2 = hailstones[2];
    long double A[6][7] = {{0}};

    // Build the 6x6 system from (Pr - Pi) x (Vr - Vi) = 0
    // Pr x (V0-V1) + Vr x (P0-P1) = P0xV0 - P1xV1 (3 equations)
    // Pr x (V0-V2) + Vr x (P0-P2) = P0xV0 - P2xV2 (3 equations)
    long double dvx1 = h0.vx - h1.vx, dvy1 = h0.vy - h1.vy, dvz1 = h0.vz - h1.vz;
    long double dpx1 = h0.px - h1.px, dpy1 = h0.py - h1.py, dpz1 = h0.pz - h1.pz;
    long double dvx2 = h0.vx - h2.vx, dvy2 = h0.vy - h2.vy, dvz2 = h0.vz - h2.vz;
    long double dpx2 = h0.px - h2.px, dpy2 = h0.py - h2.py, dpz2 = h0.pz - h2.pz;

    A[0][1]=dvz1; A[0][2]=-dvy1; A[0][4]=dpz1; A[0][5]=-dpy1;
    A[1][0]=-dvz1;A[1][2]=dvx1;  A[1][3]=-dpz1;A[1][5]=dpx1;
    A[2][0]=dvy1; A[2][1]=-dvx1; A[2][3]=dpy1; A[2][4]=-dpx1;
    A[3][1]=dvz2; A[3][2]=-dvy2; A[3][4]=dpz2; A[3][5]=-dpy2;
    A[4][0]=-dvz2;A[4][2]=dvx2;  A[4][3]=-dpz2;A[4][5]=dpx2;
    A[5][0]=dvy2; A[5][1]=-dvx2; A[5][3]=dpy2; A[5][4]=-dpx2;

    A[0][6] = ((long double)h0.py*h0.vz - (long double)h0.pz*h0.vy) - ((long double)h1.py*h1.vz - (long double)h1.pz*h1.vy);
    A[1][6] = ((long double)h0.pz*h0.vx - (long double)h0.px*h0.vz) - ((long double)h1.pz*h1.vx - (long double)h1.px*h1.vz);
    A[2][6] = ((long double)h0.px*h0.vy - (long double)h0.py*h0.vx) - ((long double)h1.px*h1.vy - (long double)h1.py*h1.vx);
    A[3][6] = ((long double)h0.py*h0.vz - (long double)h0.pz*h0.vy) - ((long double)h2.py*h2.vz - (long double)h2.pz*h2.vy);
    A[4][6] = ((long double)h0.pz*h0.vx - (long double)h0.px*h0.vz) - ((long double)h2.pz*h2.vx - (long double)h2.px*h2.vz);
    A[5][6] = ((long double)h0.px*h0.vy - (long double)h0.py*h0.vx) - ((long double)h2.px*h2.vy - (long double)h2.py*h2.vx);

    solve_linear_system(A);

    // The solution vector is in the last column of A.
    // Result must be integer, so we round the floating point results.
    long long prx = roundl(A[0][6]);
    long long pry = roundl(A[1][6]);
    long long prz = roundl(A[2][6]);

    printf("Part 2: %lld\n", prx + pry + prz);
}

int main() {
    parse_input("input.txt");
    solve_part1();
    solve_part2();
    return EXIT_SUCCESS;
}
