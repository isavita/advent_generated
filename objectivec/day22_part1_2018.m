#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *inputLines = [input componentsSeparatedByString:@"\n"];
        
        int depth = [[inputLines[0] componentsSeparatedByString:@" "][1] intValue];
        NSArray *targetCoords = [[inputLines[1] componentsSeparatedByString:@" "][1] componentsSeparatedByString:@","];
        int targetX = [targetCoords[0] intValue];
        int targetY = [targetCoords[1] intValue];
        
        int riskLevel = 0;
        int caveWidth = targetX + 1;
        int caveHeight = targetY + 1;
        
        NSMutableArray *erosionLevels = [NSMutableArray array];
        
        for (int y = 0; y < caveHeight; y++) {
            NSMutableArray *row = [NSMutableArray array];
            for (int x = 0; x < caveWidth; x++) {
                int geologicIndex;
                if ((x == 0 && y == 0) || (x == targetX && y == targetY)) {
                    geologicIndex = 0;
                } else if (y == 0) {
                    geologicIndex = x * 16807;
                } else if (x == 0) {
                    geologicIndex = y * 48271;
                } else {
                    geologicIndex = [row[x - 1] intValue] * [erosionLevels[y - 1][x] intValue];
                }
                
                int erosionLevel = (geologicIndex + depth) % 20183;
                [row addObject:@(erosionLevel)];
                
                int regionType = erosionLevel % 3;
                if (regionType == 0) {
                    riskLevel += 0;
                } else if (regionType == 1) {
                    riskLevel += 1;
                } else {
                    riskLevel += 2;
                }
            }
            [erosionLevels addObject:[row copy]];
        }
        
        printf("Total risk level: %d\n", riskLevel);
    }
    return 0;
}