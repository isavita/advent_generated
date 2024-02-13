#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        for (int i = 0; i < [lines count]-1; i++) {
            for (int j = i + 1; j < [lines count]; j++) {
                int diff = 0;
                for (int k = 0; k < [lines[i] length]; k++) {
                    if ([lines[i] characterAtIndex:k] != [lines[j] characterAtIndex:k]) {
                        diff++;
                        if (diff > 1) {
                            break;
                        }
                    }
                }
                if (diff == 1) {
                    NSMutableString *common = [NSMutableString string];
                    for (int k = 0; k < [lines[i] length]; k++) {
                        if ([lines[i] characterAtIndex:k] == [lines[j] characterAtIndex:k]) {
                            [common appendFormat:@"%c", [lines[i] characterAtIndex:k]];
                        }
                    }
                    printf("%s\n", [common UTF8String]);
                    return 0;
                }
            }
        }
    }
    return 0;
}