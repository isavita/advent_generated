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
        
        int maxY = INT_MIN;
        for (int xVel = -1000; xVel <= 1000; xVel++) {
            for (int yVel = -1000; yVel <= 1000; yVel++) {
                int xPos = 0, yPos = 0, curXVel = xVel, curYVel = yVel, highestY = yPos;
                while (1) {
                    xPos += curXVel;
                    yPos += curYVel;
                    if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                        if (highestY > maxY) maxY = highestY;
                        break;
                    }
                    if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) break;
                    if (curXVel > 0) curXVel--;
                    else if (curXVel < 0) curXVel++;
                    curYVel--;
                    if (yPos > highestY) highestY = yPos;
                }
            }
        }
        NSLog(@"%d", maxY);
    }
    return 0;
}