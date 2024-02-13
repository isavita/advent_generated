#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int fabric[1000][1000] = {0};
        int count = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            NSArray *parts = [line componentsSeparatedByString:@" "];
            NSString *position = parts[2];
            NSString *size = parts[3];
            
            NSArray *positionParts = [position componentsSeparatedByString:@","];
            int left = [positionParts[0] intValue];
            int top = [positionParts[1] intValue];
            
            NSArray *sizeParts = [size componentsSeparatedByString:@"x"];
            int width = [sizeParts[0] intValue];
            int height = [sizeParts[1] intValue];
            
            for (int i = left; i < left + width; i++) {
                for (int j = top; j < top + height; j++) {
                    if (fabric[i][j] == 1) {
                        count++;
                    }
                    fabric[i][j]++;
                }
            }
        }
        
        printf("Part One: %d\n", count);
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            NSArray *parts = [line componentsSeparatedByString:@" "];
            NSString *position = parts[2];
            NSString *size = parts[3];
            int claimID = [[parts[0] substringFromIndex:1] intValue];
            
            NSArray *positionParts = [position componentsSeparatedByString:@","];
            int left = [positionParts[0] intValue];
            int top = [positionParts[1] intValue];
            
            NSArray *sizeParts = [size componentsSeparatedByString:@"x"];
            int width = [sizeParts[0] intValue];
            int height = [sizeParts[1] intValue];
            
            BOOL overlap = NO;
            
            for (int i = left; i < left + width; i++) {
                for (int j = top; j < top + height; j++) {
                    if (fabric[i][j] > 1) {
                        overlap = YES;
                        break;
                    }
                }
                if (overlap) {
                    break;
                }
            }
            
            if (!overlap) {
                printf("Part Two: %d\n", claimID);
                break;
            }
        }
    }
    return 0;
}