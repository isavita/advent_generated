#import <Foundation/Foundation.h>

NSArray* parseRange(NSString *s) {
    NSArray *split = [s componentsSeparatedByString:@"-"];
    return @[@([[split firstObject] intValue]), @([[split lastObject] intValue])];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        NSInteger count = 0;

        for (NSString *line in lines) {
            if ([line length] == 0) continue;
            NSArray *pair = [line componentsSeparatedByString:@","];
            NSArray *left = parseRange(pair[0]);
            NSArray *right = parseRange(pair[1]);

            if ([left[0] intValue] <= [right[1] intValue] && [left[1] intValue] >= [right[0] intValue]) {
                count++;
            }
        }

        NSLog(@"%ld", (long)count);
    }
    return 0;
}