#import <Foundation/Foundation.h>

static BOOL isRealRoom(NSString *room, NSString **checksum) {
    NSRange bracket = [room rangeOfString:@"["];
    if (bracket.location == NSNotFound) return NO;
    *checksum = [[room substringWithRange:NSMakeRange(bracket.location + 1, 5)] copy];

    NSMutableArray *counts = [NSMutableArray arrayWithCapacity:26];
    for (int i = 0; i < 26; i++) [counts addObject:@0];

    NSRange beforeBracket = NSMakeRange(0, bracket.location);
    for (NSUInteger i = beforeBracket.location; i < NSMaxRange(beforeBracket); i++) {
        unichar c = [room characterAtIndex:i];
        if (c >= 'a' && c <= 'z') {
            counts[c - 'a'] = @([counts[c - 'a'] intValue] + 1);
        }
    }

    NSString *expected = @"";
    for (int i = 0; i < 26; i++) {
        int max = 0;
        NSInteger idx = -1;
        for (int j = 0; j < 26; j++) {
            int v = [counts[j] intValue];
            if (v > max) { max = v; idx = j; }
        }
        if (max == 0) break;
        expected = [expected stringByAppendingFormat:@"%C", (unichar)('a' + idx)];
        counts[idx] = @0;
    }

    return [*checksum isEqualToString:[expected substringToIndex:MIN(5, expected.length)]];
}

static NSInteger sectorID(NSString *room) {
    NSRange dash = [room rangeOfString:@"-" options:NSBackwardsSearch];
    if (dash.location == NSNotFound) return 0;
    return [[room substringFromIndex:dash.location + 1] integerValue];
}

static NSString *decrypt(NSString *room, NSInteger sector) {
    NSRange bracket = [room rangeOfString:@"["];
    NSString *enc = [room substringToIndex:bracket.location];
    NSMutableString *out = [NSMutableString stringWithCapacity:enc.length];
    for (NSUInteger i = 0; i < enc.length; i++) {
        unichar c = [enc characterAtIndex:i];
        if (c == '-') { [out appendString:@" "]; continue; }
        unichar sh = 'a' + ((c - 'a' + sector) % 26);
        [out appendFormat:@"%C", sh];
    }
    return out;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:&err];
        if (!data) { NSLog(@"Error: %@", err); return 1; }

        NSArray *lines = [data componentsSeparatedByCharactersInSet:
                        [NSCharacterSet newlineCharacterSet]];
        for (NSString *line in lines) {
            if (!line.length) continue;
            NSString *chk = nil;
            if (isRealRoom(line, &chk)) {
                NSInteger id = sectorID(line);
                NSString *plain = decrypt(line, id);
                if ([plain containsString:@"northpole object"]) {
                    printf("%ld\n", (long)id);
                    break;
                }
            }
        }
    }
    return 0;
}