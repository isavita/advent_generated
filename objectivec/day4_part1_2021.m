#import <Foundation/Foundation.h>

@interface BingoBoard : NSObject
@property (nonatomic, strong) NSMutableArray<NSMutableArray<NSNumber *> *> *numbers;
@property (nonatomic, strong) NSMutableArray<NSMutableArray<NSNumber *> *> *marked;
- (void)mark:(int)number;
- (BOOL)hasWon;
- (int)unmarkedSum;
@end

@implementation BingoBoard
- (instancetype)init {
    self = [super init];
    if (self) {
        _numbers = [NSMutableArray arrayWithCapacity:5];
        _marked = [NSMutableArray arrayWithCapacity:5];
        for (int i = 0; i < 5; i++) {
            [_numbers addObject:[NSMutableArray arrayWithCapacity:5]];
            [_marked addObject:[NSMutableArray arrayWithCapacity:5]];
            for (int j = 0; j < 5; j++) {
                [_marked[i] addObject:@(NO)];
            }
        }
    }
    return self;
}

- (void)mark:(int)number {
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (self.numbers[i][j].intValue == number) {
                self.marked[i][j] = @(YES);
            }
        }
    }
}

- (BOOL)hasWon {
    for (int i = 0; i < 5; i++) {
        if ([self isRowMarked:i] || [self isColumnMarked:i]) {
            return YES;
        }
    }
    return NO;
}

- (BOOL)isRowMarked:(int)row {
    for (int j = 0; j < 5; j++) {
        if (![self.marked[row][j] boolValue]) return NO;
    }
    return YES;
}

- (BOOL)isColumnMarked:(int)column {
    for (int i = 0; i < 5; i++) {
        if (![self.marked[i][column] boolValue]) return NO;
    }
    return YES;
}

- (int)unmarkedSum {
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (![self.marked[i][j] boolValue]) {
                sum += self.numbers[i][j].intValue;
            }
        }
    }
    return sum;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSArray *numbers = [lines[0] componentsSeparatedByString:@","];
        NSMutableArray<BingoBoard *> *boards = [NSMutableArray array];
        
        for (int i = 2; i < lines.count; i += 6) {
            BingoBoard *board = [[BingoBoard alloc] init];
            for (int j = 0; j < 5; j++) {
                NSArray *lineNumbers = [[lines[i + j] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByString:@" "];
                for (NSString *num in lineNumbers) {
                    if (num.length > 0) {
                        [board.numbers[j] addObject:@(num.intValue)];
                    }
                }
            }
            [boards addObject:board];
        }

        BingoBoard *winningBoard = nil;
        int winningNumber = 0;
        
        for (NSString *number in numbers) {
            int n = number.intValue;
            for (BingoBoard *board in boards) {
                [board mark:n];
                if ([board hasWon]) {
                    winningBoard = board;
                    winningNumber = n;
                    break;
                }
            }
            if (winningBoard) break;
        }
        
        NSLog(@"%d", [winningBoard unmarkedSum] * winningNumber);
    }
    return 0;
}