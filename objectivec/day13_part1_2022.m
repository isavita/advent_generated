#import <Foundation/Foundation.h>

// Function to compare two packets
NSComparisonResult comparePackets(NSArray *left, NSArray *right) {
    for (NSUInteger i = 0; i < MAX(left.count, right.count); i++) {
        if (i >= left.count) return NSOrderedAscending;
        if (i >= right.count) return NSOrderedDescending;

        id leftValue = left[i];
        id rightValue = right[i];

        if ([leftValue isKindOfClass:[NSNumber class]] && [rightValue isKindOfClass:[NSNumber class]]) {
            NSComparisonResult result = [(NSNumber *)leftValue compare:(NSNumber *)rightValue];
            if (result != NSOrderedSame) return result;
        } else {
            NSArray *leftArray = [leftValue isKindOfClass:[NSArray class]] ? leftValue : @[leftValue];
            NSArray *rightArray = [rightValue isKindOfClass:[NSArray class]] ? rightValue : @[rightValue];
            NSComparisonResult result = comparePackets(leftArray, rightArray);
            if (result != NSOrderedSame) return result;
        }
    }
    return NSOrderedSame;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *pairs = [input componentsSeparatedByString:@"\n\n"];

        NSUInteger sumOfIndices = 0;

        for (NSUInteger i = 0; i < pairs.count; i++) {
            NSArray *pair = [pairs[i] componentsSeparatedByString:@"\n"];
            if (pair.count != 2) continue;

            NSData *leftData = [pair[0] dataUsingEncoding:NSUTF8StringEncoding];
            NSData *rightData = [pair[1] dataUsingEncoding:NSUTF8StringEncoding];

            NSArray *leftPacket = [NSJSONSerialization JSONObjectWithData:leftData options:0 error:nil];
            NSArray *rightPacket = [NSJSONSerialization JSONObjectWithData:rightData options:0 error:nil];

            if (comparePackets(leftPacket, rightPacket) == NSOrderedAscending) {
                sumOfIndices += (i + 1);
            }
        }

        // Print the result
        NSLog(@"Sum of indices: %lu", sumOfIndices);
    }
    return 0;
}