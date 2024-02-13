#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableArray *x = [NSMutableArray arrayWithObject:@(1)];
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@"noop"]) {
                [x addObject:x.lastObject];
            } else {
                int n;
                sscanf([line UTF8String], "addx %d", &n);
                [x addObject:x.lastObject];
                [x addObject:@([x.lastObject intValue] + n)];
            }
        }
        
        int sum = 0;
        for (int i = 0; i < x.count; i++) {
            if ((i - 19) % 40 == 0) {
                sum += (i + 1) * [x[i] intValue];
            }
        }
        
        printf("%d\n", sum);
    }
    
    return 0;
}