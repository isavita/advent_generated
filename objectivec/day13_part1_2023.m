#import <Foundation/Foundation.h>

@interface Pattern : NSObject
@property (nonatomic, strong) NSMutableArray<NSString *> *rows;
- (instancetype)initWithRows:(NSArray<NSString *> *)rows;
- (NSInteger)findReflection;
@end

@implementation Pattern

- (instancetype)initWithRows:(NSArray<NSString *> *)rows {
    self = [super init];
    if (self) {
        _rows = [NSMutableArray arrayWithArray:rows];
    }
    return self;
}

- (NSInteger)findReflection {
    NSInteger horizontalReflection = [self findHorizontalReflection];
    if (horizontalReflection != -1) {
        return horizontalReflection * 100;
    }

    NSInteger verticalReflection = [self findVerticalReflection];
    if (verticalReflection != -1) {
        return verticalReflection;
    }

    return 0;
}

- (NSInteger)findHorizontalReflection {
    for (NSInteger i = 0; i < self.rows.count - 1; i++) {
        BOOL isReflected = YES;
        for (NSInteger j = 0; i - j >= 0 && i + 1 + j < self.rows.count; j++) {
            if (![self.rows[i - j] isEqualToString:self.rows[i + 1 + j]]) {
                isReflected = NO;
                break;
            }
        }
        if (isReflected) {
            return i + 1;
        }
    }
    return -1;
}

- (NSInteger)findVerticalReflection {
    NSInteger numCols = self.rows[0].length;
    for (NSInteger i = 0; i < numCols - 1; i++) {
        BOOL isReflected = YES;
        for (NSInteger j = 0; i - j >= 0 && i + 1 + j < numCols; j++) {
            for (NSString *row in self.rows) {
                if ([row characterAtIndex:i - j] != [row characterAtIndex:i + 1 + j]) {
                    isReflected = NO;
                    break;
                }
            }
            if (!isReflected) {
                break;
            }
        }
        if (isReflected) {
            return i + 1;
        }
    }
    return -1;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *patterns = [content componentsSeparatedByString:@"\n\n"];

        NSInteger total = 0;
        for (NSString *pattern in patterns) {
            NSArray<NSString *> *rows = [pattern componentsSeparatedByString:@"\n"];
            Pattern *p = [[Pattern alloc] initWithRows:rows];
            total += [p findReflection];
        }

        NSLog(@"Total: %ld", (long)total);
    }
    return 0;
}