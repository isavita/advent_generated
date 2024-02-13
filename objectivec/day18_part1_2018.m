#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int rows = 50;
        int cols = 50;
        
        char area[rows][cols];
        char nextArea[rows][cols];
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                area[i][j] = [lines[i] characterAtIndex:j];
            }
        }
        
        for (int t = 0; t < 10; t++) {
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    int trees = 0;
                    int lumberyards = 0;
                    
                    for (int x = i - 1; x <= i + 1; x++) {
                        for (int y = j - 1; y <= j + 1; y++) {
                            if (x == i && y == j) {
                                continue;
                            }
                            if (x >= 0 && x < rows && y >= 0 && y < cols) {
                                if (area[x][y] == '|') {
                                    trees++;
                                } else if (area[x][y] == '#') {
                                    lumberyards++;
                                }
                            }
                        }
                    }
                    
                    if (area[i][j] == '.' && trees >= 3) {
                        nextArea[i][j] = '|';
                    } else if (area[i][j] == '|' && lumberyards >= 3) {
                        nextArea[i][j] = '#';
                    } else if (area[i][j] == '#' && (trees == 0 || lumberyards == 0)) {
                        nextArea[i][j] = '.';
                    } else {
                        nextArea[i][j] = area[i][j];
                    }
                }
            }
            
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    area[i][j] = nextArea[i][j];
                }
            }
        }
        
        int woodedAcres = 0;
        int lumberyards = 0;
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (area[i][j] == '|') {
                    woodedAcres++;
                } else if (area[i][j] == '#') {
                    lumberyards++;
                }
            }
        }
        
        printf("Total resource value: %d\n", woodedAcres * lumberyards);
    }
    return 0;
}