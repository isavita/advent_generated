
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        if (!content) return 1;
        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"mul\\((\\d+),(\\d+)\\)"
                                                                                options:0
                                                                                  error:nil];
        __block long long total = 0;
        [regex enumerateMatchesInString:content
                                options:0
                                  range:NSMakeRange(0, content.length)
                             usingBlock:^(NSTextCheckingResult *result, NSMatchingFlags flags, BOOL *stop) {
            long long x = [[content substringWithRange:[result rangeAtIndex:1]] longLongValue];
            long long y = [[content substringWithRange:[result rangeAtIndex:2]] longLongValue];
            total += x * y;
        }];
        printf("%lld\n", total);
    }
    return 0;
}
