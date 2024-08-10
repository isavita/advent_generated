#import <Foundation/Foundation.h>

#define GRID_SIZE 10

void incrementEnergyLevel(int grid[GRID_SIZE][GRID_SIZE], int row, int col) {
    if (row >= 0 && row < GRID_SIZE && col >= 0 && col < GRID_SIZE) {
        grid[row][col]++;
    }
}

void flash(int grid[GRID_SIZE][GRID_SIZE], bool flashed[GRID_SIZE][GRID_SIZE], int row, int col, int *flashCount) {
    if (row < 0 || row >= GRID_SIZE || col < 0 || col >= GRID_SIZE || flashed[row][col]) {
        return;
    }

    flashed[row][col] = true;
    (*flashCount)++;

    // Increase energy level of all adjacent octopuses
    for (int i = -1; i <= 1; i++) {
        for (int j = -1; j <= 1; j++) {
            if (i == 0 && j == 0) continue;
            incrementEnergyLevel(grid, row + i, col + j);
        }
    }
}

void processStep(int grid[GRID_SIZE][GRID_SIZE], int *flashCount) {
    bool flashed[GRID_SIZE][GRID_SIZE] = { false };

    // Step 1: Increase energy level of each octopus by 1
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            grid[i][j]++;
        }
    }

    // Step 2: Process flashes
    bool moreFlashes = true;
    while (moreFlashes) {
        moreFlashes = false;
        for (int i = 0; i < GRID_SIZE; i++) {
            for (int j = 0; j < GRID_SIZE; j++) {
                if (grid[i][j] > 9 && !flashed[i][j]) {
                    flash(grid, flashed, i, j, flashCount);
                    moreFlashes = true;
                }
            }
        }
    }

    // Step 3: Reset flashed octopuses to 0
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (flashed[i][j]) {
                grid[i][j] = 0;
            }
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];

        int grid[GRID_SIZE][GRID_SIZE] = { 0 };

        for (int i = 0; i < GRID_SIZE; i++) {
            NSString *line = lines[i];
            for (int j = 0; j < GRID_SIZE; j++) {
                grid[i][j] = [[line substringWithRange:NSMakeRange(j, 1)] intValue];
            }
        }

        int flashCount = 0;
        for (int step = 0; step < 100; step++) {
            processStep(grid, &flashCount);
        }

        printf("Total flashes after 100 steps: %d\n", flashCount);
    }
    return 0;
}