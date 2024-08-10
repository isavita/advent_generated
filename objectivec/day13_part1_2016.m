#import <Foundation/Foundation.h>

// Function to count the number of 1 bits in the binary representation of a number
int countBits(int number) {
    int count = 0;
    while (number) {
        count += number & 1;
        number >>= 1;
    }
    return count;
}

// Function to determine if a coordinate is a wall or open space
BOOL isWall(int x, int y, int favoriteNumber) {
    int value = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    return countBits(value) % 2 == 1;
}

// Breadth-First Search (BFS) to find the shortest path
int shortestPath(int targetX, int targetY, int favoriteNumber) {
    int directions[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
    NSMutableArray *queue = [NSMutableArray array];
    NSMutableSet *visited = [NSMutableSet set];

    [queue addObject:@[@1, @1, @0]]; // Start at (1,1) with 0 steps
    [visited addObject:[NSString stringWithFormat:@"%d,%d", 1, 1]];

    while ([queue count] > 0) {
        NSArray *current = [queue objectAtIndex:0];
        [queue removeObjectAtIndex:0];

        int x = [[current objectAtIndex:0] intValue];
        int y = [[current objectAtIndex:1] intValue];
        int steps = [[current objectAtIndex:2] intValue];

        if (x == targetX && y == targetY) {
            return steps;
        }

        for (int i = 0; i < 4; i++) {
            int newX = x + directions[i][0];
            int newY = y + directions[i][1];

            if (newX >= 0 && newY >= 0 && !isWall(newX, newY, favoriteNumber)) {
                NSString *key = [NSString stringWithFormat:@"%d,%d", newX, newY];
                if (![visited containsObject:key]) {
                    [visited addObject:key];
                    [queue addObject:@[@(newX), @(newY), @(steps + 1)]];
                }
            }
        }
    }

    return -1; // If no path is found
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read the favorite number from input.txt
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        int favoriteNumber = [fileContent intValue];

        // Find the shortest path to (31,39)
        int steps = shortestPath(31, 39, favoriteNumber);

        // Print the result
        printf("Fewest number of steps required: %d\n", steps);
    }
    return 0;
}