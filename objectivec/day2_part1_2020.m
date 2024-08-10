#import <Foundation/Foundation.h>

BOOL validatePassword(NSString *policy, NSString *password) {
    NSArray *parts = [policy componentsSeparatedByString:@" "];
    NSArray *range = [[parts firstObject] componentsSeparatedByString:@"-"];
    NSInteger min = [range[0] integerValue];
    NSInteger max = [range[1] integerValue];
    unichar character = [parts[1] characterAtIndex:0];
    
    NSInteger count = 0;
    for (NSUInteger i = 0; i < password.length; i++) {
        if ([password characterAtIndex:i] == character) count++;
    }
    return count >= min && count <= max;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSInteger validCount = 0;
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        
        if (error) {
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        for (NSString *line in lines) {
            if ([line length] > 0) {
                NSRange colonRange = [line rangeOfString:@":"];
                if (colonRange.location != NSNotFound) {
                    NSString *policy = [line substringToIndex:colonRange.location];
                    NSString *password = [line substringFromIndex:colonRange.location + 2];
                    if (validatePassword(policy, password)) {
                        validCount++;
                    }
                }
            }
        }
        
        NSLog(@"%ld", (long)validCount);
    }
    return 0;
}