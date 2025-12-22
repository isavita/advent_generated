
#import <Foundation/Foundation.h>

static NSString *rotatePattern(NSString *p) {
    NSArray *rows = [p componentsSeparatedByString:@"/"];
    NSUInteger n = rows.count;
    NSMutableArray *newRows = [NSMutableArray arrayWithCapacity:n];
    for (NSUInteger x = 0; x < n; ++x) {
        NSMutableString *row = [NSMutableString string];
        for (NSInteger y = n - 1; y >= 0; --y) {
            NSString *r = rows[y];
            [row appendString:[r substringWithRange:NSMakeRange(x, 1)]];
        }
        [newRows addObject:row];
    }
    return [newRows componentsJoinedByString:@"/"];
}

static NSString *flipPattern(NSString *p) {
    NSArray *rows = [p componentsSeparatedByString:@"/"];
    NSMutableArray *newRows = [NSMutableArray arrayWithCapacity:rows.count];
    for (NSString *r in rows) {
        NSMutableString *rev = [NSMutableString stringWithString:r];
        NSUInteger i = 0, j = rev.length - 1;
        while (i < j) {
            unichar a = [rev characterAtIndex:i];
            unichar b = [rev characterAtIndex:j];
            [rev replaceCharactersInRange:NSMakeRange(i, 1) withString:[NSString stringWithCharacters:&b length:1]];
            [rev replaceCharactersInRange:NSMakeRange(j, 1) withString:[NSString stringWithCharacters:&a length:1]];
            ++i; --j;
        }
        [newRows addObject:rev];
    }
    return [newRows componentsJoinedByString:@"/"];
}

static NSString *enhancePattern(NSString *p, NSDictionary *rules, NSMutableDictionary *memo) {
    NSString *cached = memo[p];
    if (cached) return cached;
    NSString *cur = p;
    for (int i = 0; i < 4; ++i) {
        NSString *out = rules[cur];
        if (out) { memo[p] = out; return out; }
        cur = rotatePattern(cur);
    }
    cur = flipPattern(p);
    for (int i = 0; i < 4; ++i) {
        NSString *out = rules[cur];
        if (out) { memo[p] = out; return out; }
        cur = rotatePattern(cur);
    }
    @throw [NSException exceptionWithName:@"NoRule" reason:p userInfo:nil];
}

static NSString *patternFromGrid(NSArray<NSString *> *grid, NSUInteger x, NSUInteger y, NSUInteger size) {
    NSMutableArray *rows = [NSMutableArray arrayWithCapacity:size];
    for (NSUInteger dy = 0; dy < size; ++dy) {
        NSString *row = grid[y + dy];
        [rows addObject:[row substringWithRange:NSMakeRange(x, size)]];
    }
    return [rows componentsJoinedByString:@"/"];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSMutableDictionary *rules = [NSMutableDictionary dictionary];
        for (NSString *line in [input componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            NSArray *parts = [line componentsSeparatedByString:@" => "];
            if (parts.count == 2) rules[parts[0]] = parts[1];
        }
        NSMutableDictionary *memo = [NSMutableDictionary dictionary];
        NSMutableArray<NSString *> *grid = [NSMutableArray arrayWithObjects:@".#.", @"..#", @"###", nil];
        for (int iter = 0; iter < 18; ++iter) {
            NSUInteger size = grid.count;
            NSUInteger subSize = (size % 2 == 0) ? 2 : 3;
            NSUInteger newSubSize = subSize + 1;
            NSUInteger blocks = size / subSize;
            NSUInteger newSize = blocks * newSubSize;
            NSMutableArray<NSMutableString *> *newGrid = [NSMutableArray arrayWithCapacity:newSize];
            for (NSUInteger i = 0; i < newSize; ++i) {
                NSMutableString *row = [NSMutableString string];
                for (NSUInteger j = 0; j < newSize; ++j) [row appendString:@"."];
                [newGrid addObject:row];
            }
            for (NSUInteger by = 0; by < blocks; ++by) {
                for (NSUInteger bx = 0; bx < blocks; ++bx) {
                    NSString *subPat = patternFromGrid(grid, bx * subSize, by * subSize, subSize);
                    NSString *enhanced = enhancePattern(subPat, rules, memo);
                    NSArray *enhRows = [enhanced componentsSeparatedByString:@"/"];
                    for (NSUInteger dy = 0; dy < newSubSize; ++dy) {
                        NSMutableString *targetRow = newGrid[by * newSubSize + dy];
                        NSString *src = enhRows[dy];
                        NSRange range = NSMakeRange(bx * newSubSize, newSubSize);
                        [targetRow replaceCharactersInRange:range withString:src];
                    }
                }
            }
            grid = [newGrid copy];
        }
        NSUInteger count = 0;
        for (NSString *row in grid) {
            for (NSUInteger i = 0; i < row.length; ++i) if ([row characterAtIndex:i] == '#') ++count;
        }
        printf("%lu\n", (unsigned long)count);
    }
    return 0;
}
