
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <limits.h>
#import <string.h>

typedef struct {int x,y,vx,vy;} Star;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSMutableArray *stars = [NSMutableArray array];
        for (NSString *line in [data componentsSeparatedByCharactersInSet:
                                [NSCharacterSet newlineCharacterSet]]) {
            int x,y,vx,vy;
            if (sscanf([line UTF8String],
                       " position=< %d, %d> velocity=< %d, %d>", &x,&y,&vx,&vy) == 4) {
                Star s = {x,y,vx,vy};
                [stars addObject:[NSValue valueWithBytes:&s objCType:@encode(Star)]];
            }
        }

        int smallestT = 0, smallestArea = INT_MAX;
        for (int t = 1; t < 100000; ++t) {
            int maxX = INT_MIN, maxY = INT_MIN, minX = INT_MAX, minY = INT_MAX;
            for (NSValue *v in stars) {
                Star s; [v getValue:&s];
                int x = s.x + s.vx * t;
                int y = s.y + s.vy * t;
                if (x > maxX) maxX = x;
                if (y > maxY) maxY = y;
                if (x < minX) minX = x;
                if (y < minY) minY = y;
            }
            int area = (maxX - minX + 1) + (maxY - minY + 1);
            if (area < smallestArea) { smallestArea = area; smallestT = t; }
        }

        int maxX = INT_MIN, maxY = INT_MIN, minX = INT_MAX, minY = INT_MAX;
        for (NSUInteger i = 0; i < stars.count; ++i) {
            Star s; [stars[i] getValue:&s];
            s.x += s.vx * smallestT;
            s.y += s.vy * smallestT;
            stars[i] = [NSValue valueWithBytes:&s objCType:@encode(Star)];
            if (s.x > maxX) maxX = s.x;
            if (s.y > maxY) maxY = s.y;
            if (s.x < minX) minX = s.x;
            if (s.y < minY) minY = s.y;
        }

        int rows = maxY - minY + 1;
        int cols = maxX - minX + 1;
        char **grid = malloc(rows * sizeof(char *));
        for (int i = 0; i < rows; ++i) {
            grid[i] = malloc(cols + 1);
            memset(grid[i], ' ', cols);
            grid[i][cols] = '\0';
        }

        for (NSValue *v in stars) {
            Star s; [v getValue:&s];
            grid[s.y - minY][s.x - minX] = '#';
        }

        for (int i = 0; i < rows; ++i) {
            printf("%s\n", grid[i]);
            free(grid[i]);
        }
        free(grid);
    }
    return 0;
}
