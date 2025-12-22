#import <Foundation/Foundation.h>

static NSString *sortWord(NSString *w) {
    unichar buf[w.length];
    [w getCharacters:buf range:NSMakeRange(0, w.length)];
    qsort_b(buf, w.length, sizeof(unichar), ^int(const void *a, const void *b) {
        unichar ua = *(const unichar *)a;
        unichar ub = *(const unichar *)b;
        return ua - ub;
    });
    return [NSString stringWithCharacters:buf length:w.length];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!data) return 1;

        NSArray *lines = [data componentsSeparatedByString:@"\n"];
        NSUInteger validCount = 0;

        for (NSString *line in lines) {
            if (line.length == 0) continue;

            NSMutableSet *seen = [NSMutableSet set];
            BOOL valid = YES;

            for (NSString *word in [line componentsSeparatedByString:@" "]) {
                if (word.length == 0) continue;
                NSString *key = sortWord(word);
                if ([seen containsObject:key]) {
                    valid = NO;
                    break;
                }
                [seen addObject:key];
            }
            if (valid) validCount++;
        }

        printf("%lu\n", (unsigned long)validCount);
        return 0;
    }
}