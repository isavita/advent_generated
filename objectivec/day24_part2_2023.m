
#import <Foundation/Foundation.h>
#import <math.h>

typedef struct {
    long long px, py, pz;
    long long vx, vy, vz;
} Hailstone;

static Hailstone hailstones[500];
static int numHailstones = 0;

static void parseInput(void) {
    NSString *path = @"input.txt";
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
    for (NSString *line in [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
        if (numHailstones >= 500) break;
        long long px, py, pz, vx, vy, vz;
        if (sscanf(line.UTF8String, "%lld, %lld, %lld @ %lld, %lld, %lld", &px, &py, &pz, &vx, &vy, &vz) == 6) {
            hailstones[numHailstones++] = (Hailstone){px, py, pz, vx, vy, vz};
        }
    }
}

static void solvePart1(void) {
    long long count = 0;
    const long double min = 200000000000000.0L, max = 400000000000000.0L;
    for (int i = 0; i < numHailstones; ++i) {
        for (int j = i + 1; j < numHailstones; ++j) {
            Hailstone h1 = hailstones[i], h2 = hailstones[j];
            long long det = h1.vx * h2.vy - h1.vy * h2.vx;
            if (!det) continue;
            long double t1 = ((long double)(h2.px - h1.px) * h2.vy - (long double)(h2.py - h1.py) * h2.vx) / det;
            long double t2 = ((long double)(h2.px - h1.px) * h1.vy - (long double)(h2.py - h1.py) * h1.vx) / det;
            if (t1 > 0 && t2 > 0) {
                long double ix = (long double)h1.px + h1.vx * t1;
                long double iy = (long double)h1.py + h1.vy * t1;
                if (ix >= min && ix <= max && iy >= min && iy <= max) ++count;
            }
        }
    }
    printf("Part 1: %lld\n", count);
}

static void solveLinearSystem(long double A[6][7]) {
    for (int i = 0; i < 6; ++i) {
        int piv = i;
        for (int k = i + 1; k < 6; ++k) if (fabsl(A[k][i]) > fabsl(A[piv][i])) piv = k;
        for (int k = i; k < 7; ++k) { long double t = A[i][k]; A[i][k] = A[piv][k]; A[piv][k] = t; }
        for (int k = i + 1; k < 6; ++k) {
            long double f = A[k][i] / A[i][i];
            for (int j = i; j < 7; ++j) A[k][j] -= f * A[i][j];
        }
    }
    for (int i = 5; i >= 0; --i) {
        for (int j = i + 1; j < 6; ++j) A[i][6] -= A[i][j] * A[j][6];
        A[i][6] /= A[i][i];
    }
}

static void solvePart2(void) {
    if (numHailstones < 3) { printf("Part 2: Not enough data points.\n"); return; }
    Hailstone h0 = hailstones[0], h1 = hailstones[1], h2 = hailstones[2];
    long double A[6][7] = {{0}};
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

    A[0][6] = (long double)h0.py*h0.vz - (long double)h0.pz*h0.vy - ((long double)h1.py*h1.vz - (long double)h1.pz*h1.vy);
    A[1][6] = (long double)h0.pz*h0.vx - (long double)h0.px*h0.vz - ((long double)h1.pz*h1.vx - (long double)h1.px*h1.vz);
    A[2][6] = (long double)h0.px*h0.vy - (long double)h0.py*h0.vx - ((long double)h1.px*h1.vy - (long double)h1.py*h1.vx);
    A[3][6] = (long double)h0.py*h0.vz - (long double)h0.pz*h0.vy - ((long double)h2.py*h2.vz - (long double)h2.pz*h2.vy);
    A[4][6] = (long double)h0.pz*h0.vx - (long double)h0.px*h0.vz - ((long double)h2.pz*h2.vx - (long double)h2.px*h2.vz);
    A[5][6] = (long double)h0.px*h0.vy - (long double)h0.py*h0.vx - ((long double)h2.px*h2.vy - (long double)h2.py*h2.vx);

    solveLinearSystem(A);
    long long prx = llroundl(A[0][6]), pry = llroundl(A[1][6]), prz = llroundl(A[2][6]);
    printf("Part 2: %lld\n", prx + pry + prz);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        parseInput();
        solvePart1();
        solvePart2();
    }
    return 0;
}
