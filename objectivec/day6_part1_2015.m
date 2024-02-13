#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        int lights[1000][1000] = {0};
        
        for (NSString *instruction in instructions) {
            NSArray *components = [instruction componentsSeparatedByString:@" "];
            if ([components[0] isEqualToString:@"turn"]) {
                NSString *action = components[1];
                NSArray *start = [components[2] componentsSeparatedByString:@","];
                NSArray *end = [components[4] componentsSeparatedByString:@","];
                int startX = [start[0] intValue];
                int startY = [start[1] intValue];
                int endX = [end[0] intValue];
                int endY = [end[1] intValue];
                
                for (int i = startX; i <= endX; i++) {
                    for (int j = startY; j <= endY; j++) {
                        if ([action isEqualToString:@"on"]) {
                            lights[i][j] = 1;
                        } else {
                            lights[i][j] = 0;
                        }
                    }
                }
            } else if ([components[0] isEqualToString:@"toggle"]) {
                NSArray *start = [components[1] componentsSeparatedByString:@","];
                NSArray *end = [components[3] componentsSeparatedByString:@","];
                int startX = [start[0] intValue];
                int startY = [start[1] intValue];
                int endX = [end[0] intValue];
                int endY = [end[1] intValue];
                
                for (int i = startX; i <= endX; i++) {
                    for (int j = startY; j <= endY; j++) {
                        lights[i][j] = !lights[i][j];
                    }
                }
            }
        }
        
        int count = 0;
        for (int i = 0; i < 1000; i++) {
            for (int j = 0; j < 1000; j++) {
                if (lights[i][j] == 1) {
                    count++;
                }
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}