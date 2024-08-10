#import <Foundation/Foundation.h>

// Function to tilt the platform north
void tiltNorth(NSMutableArray<NSMutableArray<NSString*>*>* grid) {
    int rows = grid.count;
    int cols = grid[0].count;

    for (int col = 0; col < cols; col++) {
        int targetRow = 0;
        for (int row = 0; row < rows; row++) {
            if ([grid[row][col] isEqualToString:@"O"]) {
                // Move 'O' to the target row
                grid[row][col] = @".";
                grid[targetRow][col] = @"O";
                targetRow++;
            } else if ([grid[row][col] isEqualToString:@"#"]) {
                // Update target row to the next available row after the cube rock
                targetRow = row + 1;
            }
        }
    }
}

// Function to calculate the total load on the north support beams
int calculateLoad(NSArray<NSArray<NSString*>*>* grid) {
    int rows = grid.count;
    int totalLoad = 0;

    for (int row = 0; row < rows; row++) {
        int load = rows - row;
        for (NSString *cell in grid[row]) {
            if ([cell isEqualToString:@"O"]) {
                totalLoad += load;
            }
        }
    }

    return totalLoad;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString*> *lines = [fileContent componentsSeparatedByString:@"\n"];

        // Create a 2D grid from the input
        NSMutableArray<NSMutableArray<NSString*>*> *grid = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length > 0) {
                NSMutableArray<NSString*> *row = [NSMutableArray array];
                for (int i = 0; i < line.length; i++) {
                    [row addObject:[line substringWithRange:NSMakeRange(i, 1)]];
                }
                [grid addObject:row];
            }
        }

        // Tilt the platform north
        tiltNorth(grid);

        // Calculate the total load
        int totalLoad = calculateLoad(grid);

        // Print the total load
        NSLog(@"Total load on the north support beams: %d", totalLoad);
    }
    return 0;
}