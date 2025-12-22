#import <Foundation/Foundation.h>

static NSString *addStr(NSString *a, NSString *b) {
    const char *ca = [a UTF8String];
    const char *cb = [b UTF8String];
    size_t la = strlen(ca), lb = strlen(cb);
    size_t l = la > lb ? la : lb;
    char *res = malloc(l + 2);
    size_t i = 0, carry = 0;
    while (i < la || i < lb || carry) {
        int da = i < la ? ca[la - 1 - i] - '0' : 0;
        int db = i < lb ? cb[lb - 1 - i] - '0' : 0;
        int sum = da + db + carry;
        res[l - i] = (sum % 10) + '0';
        carry = sum / 10;
        i++;
    }
    size_t start = l + 1 - i;
    memmove(res, res + start, i + 1);
    NSString *result = [NSString stringWithUTF8String:res];
    free(res);
    return result;
}

static NSString *mulStr(NSString *a, NSString *b) {
    const char *ca = [a UTF8String];
    const char *cb = [b UTF8String];
    size_t la = strlen(ca), lb = strlen(cb);
    if ((la == 1 && ca[0] == '0') || (lb == 1 && cb[0] == '0')) return @"0";
    int *tmp = calloc(la + lb, sizeof(int));
    for (size_t i = 0; i < la; ++i) {
        int da = ca[la - 1 - i] - '0';
        for (size_t j = 0; j < lb; ++j) {
            int db = cb[lb - 1 - j] - '0';
            tmp[i + j] += da * db;
        }
    }
    int carry = 0;
    for (size_t k = 0; k < la + lb; ++k) {
        int sum = tmp[k] + carry;
        tmp[k] = sum % 10;
        carry = sum / 10;
    }
    size_t len = la + lb;
    while (len > 1 && tmp[len - 1] == 0) len--;
    char *res = malloc(len + 1);
    for (size_t i = 0; i < len; ++i) res[i] = tmp[len - 1 - i] + '0';
    res[len] = '\0';
    NSString *result = [NSString stringWithUTF8String:res];
    free(res);
    free(tmp);
    return result;
}

static void processBlock(NSArray<NSString *> *lines, NSUInteger linecnt, NSInteger start, NSInteger end, NSString **grandTotal) {
    NSMutableArray<NSString *> *nums = [NSMutableArray array];
    char op = '+';
    for (NSInteger c = start; c <= end; ++c) {
        NSMutableString *buf = [NSMutableString string];
        for (NSUInteger r = 0; r < linecnt; ++r) {
            NSString *ln = lines[r];
            if (c < (NSInteger)[ln length]) {
                unichar ch = [ln characterAtIndex:c];
                if (ch >= '0' && ch <= '9')
                    [buf appendFormat:@"%C", ch];
                else if (ch == '+' || ch == '*')
                    op = (char)ch;
            }
        }
        if ([buf length] > 0) [nums addObject:buf];
    }
    if ([nums count] == 0) return;
    NSString *blockRes = (op == '*') ? @"1" : @"0";
    for (NSString *num in nums) {
        blockRes = (op == '*') ? mulStr(blockRes, num) : addStr(blockRes, num);
    }
    NSString *newTotal = addStr(*grandTotal, blockRes);
    *grandTotal = newTotal;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        NSMutableArray<NSString *> *lines = [NSMutableArray array];
        NSUInteger maxw = 0;
        for (NSString *rawLine in [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
            NSString *line = [rawLine stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@"\r\n"]];
            [lines addObject:line];
            if ([line length] > maxw) maxw = [line length];
        }
        NSUInteger linecnt = [lines count];
        if (linecnt == 0) {
            printf("Grand total: 0\n");
            return 0;
        }
        BOOL *isSep = calloc(maxw, sizeof(BOOL));
        for (NSUInteger x = 0; x < maxw; ++x) {
            BOOL allspace = YES;
            for (NSUInteger r = 0; r < linecnt; ++r) {
                NSString *ln = lines[r];
                if (x < [ln length] && ![[ln substringWithRange:NSMakeRange(x, 1)] isEqualToString:@" "]) {
                    allspace = NO;
                    break;
                }
            }
            isSep[x] = allspace;
        }
        NSString *grandTotal = @"0";
        BOOL inBlock = NO;
        NSInteger start = 0;
        for (NSUInteger x = 0; x < maxw; ++x) {
            if (!isSep[x]) {
                if (!inBlock) {
                    inBlock = YES;
                    start = (NSInteger)x;
                }
            } else {
                if (inBlock) {
                    processBlock(lines, linecnt, start, (NSInteger)x - 1, &grandTotal);
                    inBlock = NO;
                }
            }
        }
        if (inBlock) processBlock(lines, linecnt, start, (NSInteger)maxw - 1, &grandTotal);
        printf("Grand total: %s\n", [grandTotal UTF8String]);
        free(isSep);
    }
    return 0;
}