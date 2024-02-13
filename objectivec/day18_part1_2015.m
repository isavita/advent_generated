#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int grid[100][100];
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                grid[i][j] = ([lines[i] characterAtIndex:j] == '#') ? 1 : 0;
            }
        }
        
        for (int step = 0; step < 100; step++) {
            int newGrid[100][100];
            for (int i = 0; i < 100; i++) {
                for (int j = 0; j < 100; j++) {
                    int neighbors = 0;
                    for (int x = -1; x <= 1; x++) {
                        for (int y = -1; y <= 1; y++) {
                            if (x == 0 && y == 0) continue;
                            int ni = i + x;
                            int nj = j + y;
                            if (ni >= 0 && ni < 100 && nj >= 0 && nj < 100) {
                                neighbors += grid[ni][nj];
                            }
                        }
                    }
                    
                    if (grid[i][j] == 1) {
                        newGrid[i][j] = (neighbors == 2 || neighbors == 3) ? 1 : 0;
                    } else {
                        newGrid[i][j] = (neighbors == 3) ? 1 : 0;
                    }
                }
            }
            
            for (int i = 0; i < 100; i++) {
                for (int j = 0; j < 100; j++) {
                    grid[i][j] = newGrid[i][j];
                }
            }
        }
        
        int totalLightsOn = 0;
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                totalLightsOn += grid[i][j];
            }
        }
        
        printf("%d\n", totalLightsOn);
    }
    return 0;
}