
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                     encoding:NSUTF8StringEncoding
                                                        error:&error];
        if (!content) return 1;

        NSInteger total = 0;
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        for (NSString *line in lines) {
            NSRange colon = [line rangeOfString:@":"];
            if (colon.location == NSNotFound) continue;
            NSString *rest = [line substringFromIndex:colon.location + 1];
            NSArray *parts = [rest componentsSeparatedByString:@"|"];
            if (parts.count < 2) continue;

            NSArray *winning = [[parts[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]]
                               componentsSeparatedByString:@" "];
            NSArray *yours = [[parts[1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]]
                             componentsSeparatedByString:@" "];

            NSMutableSet *winSet = [NSMutableSet set];
            for (NSString *s in winning) if (s.length) [winSet addObject:s];

            NSInteger points = 0;
            for (NSString *s in yours)
                if (s.length && [winSet containsObject:s])
                    points = points ? points * 2 : 1;

            total += points;
        }
        printf("%ld\n", (long)total);
    }
    return 0;
}
