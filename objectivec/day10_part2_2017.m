#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSMutableArray<NSNumber *> *lengths = [NSMutableArray array];
        for (NSUInteger i = 0; i < input.length; i++) {
            [lengths addObject:@([input characterAtIndex:i])];
        }
        [lengths addObjectsFromArray:@[@17, @31, @73, @47, @23]];

        NSMutableArray<NSNumber *> *list = [NSMutableArray arrayWithCapacity:256];
        for (int i = 0; i < 256; i++) {
            [list addObject:@(i)];
        }

        NSUInteger currentPosition = 0, skipSize = 0;
        for (int round = 0; round < 64; round++) {
            for (NSNumber *length in lengths) {
                NSUInteger len = length.unsignedIntegerValue;
                for (NSUInteger i = 0; i < len / 2; i++) {
                    NSUInteger start = (currentPosition + i) % 256;
                    NSUInteger end = (currentPosition + len - 1 - i) % 256;
                    NSNumber *temp = list[start];
                    list[start] = list[end];
                    list[end] = temp;
                }
                currentPosition = (currentPosition + len + skipSize) % 256;
                skipSize++;
            }
        }

        NSMutableArray<NSNumber *> *denseHash = [NSMutableArray array];
        for (NSUInteger i = 0; i < 256; i += 16) {
            NSUInteger xor = 0;
            for (NSUInteger j = 0; j < 16; j++) {
                xor ^= list[i + j].unsignedIntegerValue;
            }
            [denseHash addObject:@(xor)];
        }

        NSMutableString *hexHash = [NSMutableString string];
        for (NSNumber *num in denseHash) {
            [hexHash appendFormat:@"%02x", num.unsignedIntegerValue];
        }

        NSLog(@"%@", hexHash);
    }
    return 0;
}