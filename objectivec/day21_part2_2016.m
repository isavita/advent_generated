
#import <Foundation/Foundation.h>

@interface Scrambler : NSObject
@property (nonatomic, strong) NSMutableString *pw;
- (void)swapPositions:(int)x y:(int)y;
- (void)swapLetters:(char)x y:(char)y;
- (void)rotate:(int)steps;
- (void)rotateLetter:(char)x;
- (void)derotateLetter:(char)x;
- (void)reverse:(int)x y:(int)y;
- (void)move:(int)x y:(int)y;
- (void)unscramble:(NSArray<NSString *> *)inst;
@end

@implementation Scrambler

- (void)swapPositions:(int)x y:(int)y {
    unichar a = [self.pw characterAtIndex:x];
    [self.pw replaceCharactersInRange:NSMakeRange(x, 1) withString:[NSString stringWithFormat:@"%C", [self.pw characterAtIndex:y]]];
    [self.pw replaceCharactersInRange:NSMakeRange(y, 1) withString:[NSString stringWithFormat:@"%C", a]];
}
- (void)swapLetters:(char)x y:(char)y {
    int ix = (int)[self.pw rangeOfString:[NSString stringWithFormat:@"%c", x]].location;
    int iy = (int)[self.pw rangeOfString:[NSString stringWithFormat:@"%c", y]].location;
    if (ix != NSNotFound && iy != NSNotFound) [self swapPositions:ix y:iy];
}
- (void)rotate:(int)steps {
    int L = (int)self.pw.length;
    steps = (steps % L + L) % L;
    NSString *s = self.pw;
    self.pw = [NSMutableString stringWithFormat:@"%@%@",
               [s substringFromIndex:L - steps],
               [s substringToIndex:L - steps]];
}
- (void)rotateLetter:(char)x {
    int ix = (int)[self.pw rangeOfString:[NSString stringWithFormat:@"%c", x]].location;
    if (ix != NSNotFound) {
        if (ix >= 4) ix++;
        [self rotate:ix + 1];
    }
}
- (void)derotateLetter:(char)x {
    int ix = (int)[self.pw rangeOfString:[NSString stringWithFormat:@"%c", x]].location;
    int rot;
    if (ix % 2 == 1) rot = -(ix + 1) / 2;
    else if (ix != 0) rot = (6 - ix) / 2;
    else rot = -1;
    [self rotate:rot];
}
- (void)reverse:(int)x y:(int)y {
    while (x < y) {
        [self swapPositions:x y:y];
        x++; y--;
    }
}
- (void)move:(int)x y:(int)y {
    unichar ch = [self.pw characterAtIndex:x];
    [self.pw deleteCharactersInRange:NSMakeRange(x, 1)];
    [self.pw insertString:[NSString stringWithFormat:@"%C", ch] atIndex:y];
}
- (void)unscramble:(NSArray<NSString *> *)inst {
    for (NSInteger i = inst.count - 1; i >= 0; i--) {
        NSString *l = inst[i];
        NSArray *parts = [l componentsSeparatedByString:@" "];
        if ([l hasPrefix:@"swap position"]) {
            int x = [parts[2] intValue], y = [parts[5] intValue];
            [self swapPositions:x y:y];
        } else if ([l hasPrefix:@"swap letter"]) {
            char x = [parts[2] UTF8String][0], y = [parts[5] UTF8String][0];
            [self swapLetters:x y:y];
        } else if ([l hasPrefix:@"rotate based"]) {
            char x = [parts[6] UTF8String][0];
            [self derotateLetter:x];
        } else if ([l hasPrefix:@"rotate"]) {
            int steps = [parts[2] intValue];
            if ([parts[1] isEqualToString:@"left"]) steps = -steps;
            [self rotate:-steps];
        } else if ([l hasPrefix:@"reverse"]) {
            int x = [parts[2] intValue], y = [parts[4] intValue];
            [self reverse:x y:y];
        } else if ([l hasPrefix:@"move"]) {
            int x = [parts[2] intValue], y = [parts[5] intValue];
            [self move:y y:x];
        }
    }
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *inst = [txt componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        Scrambler *s = [[Scrambler alloc] init];
        s.pw = [NSMutableString stringWithString:@"fbgdceah"];
        [s unscramble:inst];
        printf("%s\n", [s.pw UTF8String]);
    }
    return 0;
}
