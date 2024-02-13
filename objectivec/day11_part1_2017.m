#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *steps = [input componentsSeparatedByString:@","];
        
        int x = 0;
        int y = 0;
        int z = 0;
        int maxDist = 0;
        
        for (NSString *step in steps) {
            if ([step isEqualToString:@"n"]) {
                y++;
                z--;
            } else if ([step isEqualToString:@"ne"]) {
                x++;
                z--;
            } else if ([step isEqualToString:@"se"]) {
                x++;
                y--;
            } else if ([step isEqualToString:@"s"]) {
                y--;
                z++;
            } else if ([step isEqualToString:@"sw"]) {
                x--;
                z++;
            } else if ([step isEqualToString:@"nw"]) {
                x--;
                y++;
            }
            
            int dist = (abs(x) + abs(y) + abs(z)) / 2;
            if (dist > maxDist) {
                maxDist = dist;
            }
        }
        
        printf("%d\n", (abs(x) + abs(y) + abs(z)) / 2);
    }
    return 0;
}