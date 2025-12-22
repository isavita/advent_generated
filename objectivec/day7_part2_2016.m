#import <Foundation/Foundation.h>

static BOOL supportsSSL(NSString *ip) {
    NSMutableArray<NSString *> *bracketParts = [NSMutableArray array];
    NSRegularExpression *re = [NSRegularExpression regularExpressionWithPattern:@"\\[([^\\]]+)\\]"
                                                                      options:0
                                                                        error:nil];
    NSString *outside = [re stringByReplacingMatchesInString:ip
                                                    options:0
                                                      range:NSMakeRange(0, ip.length)
                                               withTemplate:@""];
    [re enumerateMatchesInString:ip
                          options:0
                            range:NSMakeRange(0, ip.length)
                       usingBlock:^(NSTextCheckingResult *match, NSMatchingFlags flags, BOOL *stop) {
        [bracketParts addObject:[ip substringWithRange:[match rangeAtIndex:1]]];
    }];
    
    for (NSUInteger i = 0; i + 2 < outside.length; ++i) {
        unichar a = [outside characterAtIndex:i];
        unichar b = [outside characterAtIndex:i+1];
        unichar c = [outside characterAtIndex:i+2];
        if (a == c && a != b) {
            NSString *bab = [NSString stringWithFormat:@"%C%C%C", b, a, b];
            for (NSString *part in bracketParts) {
                if ([part containsString:bab]) return YES;
            }
        }
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
        NSUInteger sslCount = 0;
        for (NSString *line in lines) {
            NSString *trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (trimmed.length && supportsSSL(trimmed)) ++sslCount;
        }
        printf("%lu\n", (unsigned long)sslCount);
    }
    return 0;
}