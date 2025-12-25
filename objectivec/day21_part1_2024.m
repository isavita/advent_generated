#import <Foundation/Foundation.h>

typedef struct { int i, j; } Pos;

static Pos findPos(NSArray<NSString*>* pad, char ch) {
    for (NSUInteger i = 0; i < pad.count; ++i) {
        NSString* row = pad[i];
        for (NSUInteger j = 0; j < row.length; ++j) {
            if ([row characterAtIndex:j] == ch) return (Pos){(int)i, (int)j};
        }
    }
    return (Pos){-1, -1};
}

static BOOL ok(NSArray<NSString*>* pad, Pos st, NSString* seq) {
    Pos curr = st;
    for (NSUInteger k = 0; k < seq.length; ++k) {
        if ([pad[curr.i] characterAtIndex:curr.j] == ' ') return NO;
        unichar ch = [seq characterAtIndex:k];
        switch (ch) {
            case '^': curr.i--; break;
            case 'v': curr.i++; break;
            case '<': curr.j--; break;
            case '>': curr.j++; break;
        }
        if (curr.i < 0 || curr.i >= (int)pad.count ||
            curr.j < 0 || curr.j >= (int)[pad[0] length]) return NO;
    }
    return YES;
}

static NSString* genMoves(Pos from, char to, NSArray<NSString*>* pad) {
    Pos tgt = findPos(pad, to);
    NSMutableString* res = [NSMutableString string];
    if (from.j > tgt.j) [res appendString:[@"" stringByPaddingToLength:from.j-tgt.j withString:@"<" startingAtIndex:0]];
    if (from.i > tgt.i) [res appendString:[@"" stringByPaddingToLength:from.i-tgt.i withString:@"^" startingAtIndex:0]];
    if (from.i < tgt.i) [res appendString:[@"" stringByPaddingToLength:tgt.i-from.i withString:@"v" startingAtIndex:0]];
    if (from.j < tgt.j) [res appendString:[@"" stringByPaddingToLength:tgt.j-from.j withString:@">" startingAtIndex:0]];
    if (!ok(pad, from, res)) {
        [res setString:@""];
        if (from.j < tgt.j) [res appendString:[@"" stringByPaddingToLength:tgt.j-from.j withString:@">" startingAtIndex:0]];
        if (from.i > tgt.i) [res appendString:[@"" stringByPaddingToLength:from.i-tgt.i withString:@"^" startingAtIndex:0]];
        if (from.i < tgt.i) [res appendString:[@"" stringByPaddingToLength:tgt.i-from.i withString:@"v" startingAtIndex:0]];
        if (from.j > tgt.j) [res appendString:[@"" stringByPaddingToLength:from.j-tgt.j withString:@"<" startingAtIndex:0]];
    }
    return res;
}

static int solve(NSString* code, int bots, NSArray<NSString*>* kp, NSArray<NSString*>* rp, int maxBots) {
    if (bots <= 0) return (int)code.length;
    int res = 0;
    int pi = bots == maxBots ? 3 : 0, pj = bots == maxBots ? 2 : 2;
    for (NSUInteger k = 0; k < code.length; ++k) {
        char ch = [code characterAtIndex:k];
        NSString* mv = genMoves((Pos){pi, pj}, ch, bots == maxBots ? kp : rp);
        Pos np = findPos(bots == maxBots ? kp : rp, ch);
        pi = np.i; pj = np.j;
        res += solve([mv stringByAppendingString:@"A"], bots - 1, kp, rp, maxBots);
    }
    return res;
}

int main(int argc, const char* argv[]) {
    @autoreleasepool {
        NSError* err = nil;
        NSString* txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                 encoding:NSUTF8StringEncoding
                                                    error:&err];
        if (!txt) { NSLog(@"Error"); return 1; }
        NSArray<NSString*>* lines = [txt componentsSeparatedByString:@"\n"];
        NSArray<NSString*>* kp = @[@"789", @"456", @"123", @" 0A"];
        NSArray<NSString*>* rp = @[@" ^A", @"<v>"];
        int total = 0;
        for (NSString* line in lines) {
            line = [line stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
            if (line.length == 0) continue;
            int num = 0;
            for (NSUInteger i = 0; i < line.length; ++i) {
                unichar c = [line characterAtIndex:i];
                if (c >= '0' && c <= '9') num = num * 10 + (c - '0');
            }
            total += solve(line, 3, kp, rp, 3) * num;
        }
        printf("%d\n", total);
    }
    return 0;
}