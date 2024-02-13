#import <Foundation/Foundation.h>

int main() {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    NSError *error = nil;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"File reading error: %@", error);
        return 1;
    }

    int score = 0;
    int depth = 0;
    BOOL inGarbage = NO;
    BOOL cancelNext = NO;

    for (int i = 0; i < input.length; i++) {
        unichar ch = [input characterAtIndex:i];
        if (cancelNext) {
            cancelNext = NO;
            continue;
        }

        if (inGarbage) {
            if (ch == '!') {
                cancelNext = YES;
            } else if (ch == '>') {
                inGarbage = NO;
            }
        } else {
            switch (ch) {
                case '{':
                    depth++;
                    break;
                case '}':
                    score += depth;
                    depth--;
                    break;
                case '<':
                    inGarbage = YES;
                    break;
            }
        }
    }

    printf("%d\n", score);

    [pool drain];
    return 0;
}