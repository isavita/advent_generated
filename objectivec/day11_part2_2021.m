
#import <Foundation/Foundation.h>

#define SIZE 10

static int grid[SIZE][SIZE];

static void readInput(void) {
    FILE *f = fopen("input.txt", "r");
    for (int y = 0; y < SIZE; ++y)
        for (int x = 0; x < SIZE; ++x)
            fscanf(f, "%1d", &grid[y][x]);
    fclose(f);
}

static int flash(int x, int y, char flashed[SIZE][SIZE]) {
    if (flashed[y][x]) return 0;
    flashed[y][x] = 1;
    int count = 1;
    static const int dirs[8][2] = {{-1,-1},{-1,0},{-1,1},{0,-1},{0,1},{1,-1},{1,0},{1,1}};
    for (int i = 0; i < 8; ++i) {
        int nx = x + dirs[i][0], ny = y + dirs[i][1];
        if (nx >= 0 && nx < SIZE && ny >= 0 && ny < SIZE) {
            if (++grid[ny][nx] > 9)
                count += flash(nx, ny, flashed);
        }
    }
    return count;
}

static int simulateStep(void) {
    char flashed[SIZE][SIZE] = {0};
    for (int y = 0; y < SIZE; ++y)
        for (int x = 0; x < SIZE; ++x)
            ++grid[y][x];
    int flashes = 0;
    for (int y = 0; y < SIZE; ++y)
        for (int x = 0; x < SIZE; ++x)
            if (grid[y][x] > 9)
                flashes += flash(x, y, flashed);
    for (int y = 0; y < SIZE; ++y)
        for (int x = 0; x < SIZE; ++x)
            if (flashed[y][x]) grid[y][x] = 0;
    return flashes;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        readInput();
        int step = 0;
        while (simulateStep() != 100) ++step;
        printf("%d\n", step + 1);
    }
    return 0;
}
