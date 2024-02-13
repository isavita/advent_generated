#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *points = [NSMutableDictionary dictionary];
        NSMutableArray *folds = [NSMutableArray array];
        BOOL readingPoints = YES;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                readingPoints = NO;
                continue;
            }
            
            if (readingPoints) {
                NSArray *coords = [line componentsSeparatedByString:@","];
                int x = [coords[0] intValue];
                int y = [coords[1] intValue];
                NSValue *pointValue = [NSValue valueWithPoint:NSMakePoint(x, y)];
                [points setObject:@(YES) forKey:pointValue];
            } else {
                [folds addObject:line];
            }
        }
        
        NSString *fold = [[folds[0] componentsSeparatedByString:@" "] objectAtIndex:2];
        NSArray *axisValue = [fold componentsSeparatedByString:@"="];
        NSString *axis = axisValue[0];
        int value = [axisValue[1] intValue];
        
        NSMutableDictionary *newPoints = [NSMutableDictionary dictionary];
        
        if ([axis isEqualToString:@"x"]) {
            for (NSValue *pointValue in points) {
                NSPoint point = [pointValue pointValue];
                if (point.x > value) {
                    point.x = 2*value - point.x;
                }
                NSValue *newPointValue = [NSValue valueWithPoint:point];
                [newPoints setObject:@(YES) forKey:newPointValue];
            }
        } else {
            for (NSValue *pointValue in points) {
                NSPoint point = [pointValue pointValue];
                if (point.y > value) {
                    point.y = 2*value - point.y;
                }
                NSValue *newPointValue = [NSValue valueWithPoint:point];
                [newPoints setObject:@(YES) forKey:newPointValue];
            }
        }
        
        printf("%lu\n", [newPoints count]);
    }
    return 0;
}