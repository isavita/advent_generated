#import <Foundation/Foundation.h>

int calculateMemoryLength(NSString *s) {
    int length = 0;
    BOOL inEscape = NO;
    int hexCount = 0;

    for (NSUInteger i = 1; i < s.length - 1; i++) {
        unichar c = [s characterAtIndex:i];
        if (hexCount > 0) {
            hexCount--;
        } else if (inEscape) {
            if (c == 'x') hexCount = 2;
            inEscape = NO;
            length++;
        } else if (c == '\\') {
            inEscape = YES;
        } else {
            length++;
        }
    }
    return length;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }

        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int totalDiff = 0;

        for (NSString *line in lines) {
            if (line.length > 0) {
                int codeLength = (int)line.length;
                int memoryLength = calculateMemoryLength(line);
                totalDiff += codeLength - memoryLength;
            }
        }

        NSLog(@"%d", totalDiff);
    }
    return 0;
}