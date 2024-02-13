#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        int lights[1000][1000] = {0};
        
        for (NSString *instruction in instructions) {
            if ([instruction hasPrefix:@"turn on"]) {
                NSArray *components = [instruction componentsSeparatedByString:@" "];
                NSArray *start = [components[2] componentsSeparatedByString:@","];
                NSArray *end = [components[4] componentsSeparatedByString:@","];
                int startX = [start[0] intValue];
                int startY = [start[1] intValue];
                int endX = [end[0] intValue];
                int endY = [end[1] intValue];
                
                for (int i = startX; i <= endX; i++) {
                    for (int j = startY; j <= endY; j++) {
                        lights[i][j]++;
                    }
                }
            } else if ([instruction hasPrefix:@"turn off"]) {
                NSArray *components = [instruction componentsSeparatedByString:@" "];
                NSArray *start = [components[2] componentsSeparatedByString:@","];
                NSArray *end = [components[4] componentsSeparatedByString:@","];
                int startX = [start[0] intValue];
                int startY = [start[1] intValue];
                int endX = [end[0] intValue];
                int endY = [end[1] intValue];
                
                for (int i = startX; i <= endX; i++) {
                    for (int j = startY; j <= endY; j++) {
                        if (lights[i][j] > 0) {
                            lights[i][j]--;
                        }
                    }
                }
            } else if ([instruction hasPrefix:@"toggle"]) {
                NSArray *components = [instruction componentsSeparatedByString:@" "];
                NSArray *start = [components[1] componentsSeparatedByString:@","];
                NSArray *end = [components[3] componentsSeparatedByString:@","];
                int startX = [start[0] intValue];
                int startY = [start[1] intValue];
                int endX = [end[0] intValue];
                int endY = [end[1] intValue];
                
                for (int i = startX; i <= endX; i++) {
                    for (int j = startY; j <= endY; j++) {
                        lights[i][j] += 2;
                    }
                }
            }
        }
        
        int totalBrightness = 0;
        for (int i = 0; i < 1000; i++) {
            for (int j = 0; j < 1000; j++) {
                totalBrightness += lights[i][j];
            }
        }
        
        printf("%d\n", totalBrightness);
    }
    return 0;
}