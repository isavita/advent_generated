
#import <Foundation/Foundation.h>

#define MAX_SIZE 30
#define CYCLES 6

static inline int countActive(int g[MAX_SIZE][MAX_SIZE][MAX_SIZE][MAX_SIZE],
                              int x, int y, int z, int w) {
    int c = 0;
    for (int i = x - 1; i <= x + 1; ++i)
        for (int j = y - 1; j <= y + 1; ++j)
            for (int k = z - 1; k <= z + 1; ++k)
                for (int l = w - 1; l <= w + 1; ++l)
                    if ((i != x || j != y || k != z || l != w) && g[i][j][k][l])
                        ++c;
    return c;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *fp = fopen("input.txt", "r");
        if (!fp) return 1;

        static int grid[MAX_SIZE][MAX_SIZE][MAX_SIZE][MAX_SIZE] = {0};
        static int newGrid[MAX_SIZE][MAX_SIZE][MAX_SIZE][MAX_SIZE] = {0};

        int mid = MAX_SIZE / 2;
        int offset = mid - 3;
        int input[8][8];

        for (int i = 0; i < 8; ++i) {
            for (int j = 0; j < 8; ++j) {
                char c;
                fscanf(fp, " %c", &c);
                input[i][j] = (c == '#');
            }
        }
        fclose(fp);

        for (int i = 0; i < 8; ++i)
            for (int j = 0; j < 8; ++j)
                grid[mid][mid][offset + i][offset + j] = input[i][j];

        for (int cycle = 0; cycle < CYCLES; ++cycle) {
            for (int x = 1; x < MAX_SIZE - 1; ++x)
                for (int y = 1; y < MAX_SIZE - 1; ++y)
                    for (int z = 1; z < MAX_SIZE - 1; ++z)
                        for (int w = 1; w < MAX_SIZE - 1; ++w) {
                            int n = countActive(grid, x, y, z, w);
                            if (grid[x][y][z][w])
                                newGrid[x][y][z][w] = (n == 2 || n == 3);
                            else
                                newGrid[x][y][z][w] = (n == 3);
                        }
            memcpy(grid, newGrid, sizeof(grid));
            memset(newGrid, 0, sizeof(newGrid));
        }

        int total = 0;
        for (int x = 0; x < MAX_SIZE; ++x)
            for (int y = 0; y < MAX_SIZE; ++y)
                for (int z = 0; z < MAX_SIZE; ++z)
                    for (int w = 0; w < MAX_SIZE; ++w)
                        total += grid[x][y][z][w];

        printf("%d\n", total);
    }
    return 0;
}
