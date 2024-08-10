#import <Foundation/Foundation.h>

BOOL isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
    return (xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *parts = [input componentsSeparatedByString:@", "];
        NSArray *xRange = [[parts[0] substringFromIndex:15] componentsSeparatedByString:@".."];
        NSArray *yRange = [[parts[1] substringFromIndex:2] componentsSeparatedByString:@".."];
        
        int xMin = [xRange[0] intValue], xMax = [xRange[1] intValue];
        int yMin = [yRange[0] intValue], yMax = [yRange[1] intValue];
        
        NSMutableSet *velocities = [NSMutableSet set];
        
        for (int xVel = -1000; xVel <= 1000; xVel++) {
            for (int yVel = -1000; yVel <= 1000; yVel++) {
                int xPos = 0, yPos = 0, curXVel = xVel, curYVel = yVel;
                BOOL inTargetArea = NO;
                
                while (1) {
                    xPos += curXVel;
                    yPos += curYVel;
                    
                    if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                        inTargetArea = YES;
                        break;
                    }
                    
                    if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
                        break;
                    }
                    
                    curXVel += (curXVel > 0) ? -1 : (curXVel < 0) ? 1 : 0;
                    curYVel--;
                }
                
                if (inTargetArea) {
                    [velocities addObject:[NSString stringWithFormat:@"%d,%d", xVel, yVel]];
                }
            }
        }
        
        NSLog(@"%lu", (unsigned long)[velocities count]);
    }
    return 0;
}