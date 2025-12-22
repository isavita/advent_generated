
#import <Foundation/Foundation.h>

static int calc(NSString *s) {
    NSUInteger len = s.length;
    for (int d1 = 9; d1 >= 0; --d1) {
        unichar c = (unichar)('0' + d1);
        NSRange r = [s rangeOfString:[NSString stringWithFormat:@"%C", c]];
        if (r.location == NSNotFound || r.location + 1 >= len) continue;
        int max2 = -1;
        for (NSUInteger i = r.location + 1; i < len; ++i) {
            unichar ch = [s characterAtIndex:i];
            if (ch >= '0' && ch <= '9') {
                int v = ch - '0';
                if (v > max2) max2 = v;
                if (max2 == 9) break;
            }
        }
        if (max2 != -1) return d1 * 10 + max2;
    }
    return 0;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int total = 0;
        for (NSString *line in lines) {
            if (line.length) total += calc(line);
        }
        printf("%d\n", total);
    }
    return 0;
}
