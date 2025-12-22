
#import <Foundation/Foundation.h>

static NSString *trimLeadingZeros(NSString *s) {
    NSUInteger len = s.length;
    NSUInteger i = 0;
    while (i < len - 1 && [s characterAtIndex:i] == '0') i++;
    return [s substringFromIndex:i];
}

static void splitStone(NSString *s, NSString **left, NSString **right) {
    NSUInteger len = s.length;
    NSUInteger mid = len / 2;
    NSString *l = [s substringToIndex:mid];
    NSString *r = [s substringFromIndex:mid];
    *left = trimLeadingZeros(l);
    *right = trimLeadingZeros(r);
}

static NSString *multiplyBy2024(NSString *s) {
    if (s.length == 0) return @"0";
    const int mult[4] = {2,0,2,4};
    NSUInteger slen = s.length;
    NSUInteger maxLen = slen + 4;
    int *res = calloc(maxLen, sizeof(int));
    for (NSUInteger i = 0; i < slen; i++) {
        int sd = [s characterAtIndex:slen - 1 - i] - '0';
        int carry = 0;
        for (NSUInteger j = 0; j < 4; j++) {
            int prod = sd * mult[3 - j] + res[i + j] + carry;
            res[i + j] = prod % 10;
            carry = prod / 10;
        }
        NSUInteger k = i + 4;
        while (carry) {
            int sum = res[k] + carry;
            res[k] = sum % 10;
            carry = sum / 10;
            k++;
        }
    }
    NSUInteger idx = maxLen - 1;
    while (idx > 0 && res[idx] == 0) idx--;
    NSMutableString *out = [NSMutableString stringWithCapacity:idx + 1];
    for (NSInteger i = idx; i >= 0; i--) [out appendFormat:@"%c", (char)(res[i] + '0')];
    free(res);
    return out;
}

static void addToMap(NSMutableDictionary<NSString*, NSNumber*> *map, NSString *key, unsigned long long inc) {
    NSNumber *old = map[key];
    unsigned long long val = old ? old.unsignedLongLongValue + inc : inc;
    map[key] = @(val);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!input) return 1;
        input = [input stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSArray<NSString*> *tokens = [input componentsSeparatedByString:@" "];
        NSMutableDictionary<NSString*, NSNumber*> *current = [NSMutableDictionary dictionary];
        NSMutableDictionary<NSString*, NSNumber*> *next = [NSMutableDictionary dictionary];
        for (NSString *tok in tokens) if (tok.length) addToMap(current, tok, 1);
        const int STEPS = 75;
        for (int step = 0; step < STEPS; step++) {
            [next removeAllObjects];
            for (NSString *stone in current) {
                unsigned long long cnt = current[stone].unsignedLongLongValue;
                if (stone.length == 1 && [stone isEqualToString:@"0"]) {
                    addToMap(next, @"1", cnt);
                } else if (stone.length % 2 == 0) {
                    NSString *l, *r;
                    splitStone(stone, &l, &r);
                    addToMap(next, l, cnt);
                    addToMap(next, r, cnt);
                } else {
                    NSString *newStone = multiplyBy2024(stone);
                    addToMap(next, newStone, cnt);
                }
            }
            NSMutableDictionary *tmp = current;
            current = next;
            next = tmp;
        }
        unsigned long long total = 0;
        for (NSNumber *v in current.allValues) total += v.unsignedLongLongValue;
        printf("%llu\n", total);
    }
    return 0;
}
